{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception  ( bracket )
import Control.Lens.Operators

import Data.Acid
import Data.Acid.Local    ( createCheckpointAndClose )
import Data.Acid.Advanced ( query', update' )
import Data.Time          ( getCurrentTime )

import Happstack.Server
import Happstack.Server.SimpleHTTPS
import Happstack.Server.Compression

import HTML.Base
import HTML.Index
import HTML.File
import HTML.Upload
import HTML.Download
import FileUtils
import UploadDB
import qualified HTML.Error as Error

httpConf :: Conf
httpConf = nullConf { port = 8082 }

httpsConf :: TLSConf
httpsConf = nullTLSConf
  { tlsPort = 8083
  , tlsCert = "nils.cc.crt"
  , tlsKey  = "nils.key"
  }

uploadDir :: FilePath
uploadDir = "files/"

main :: IO ()
main = do
  putStrLn "Starting server..."

  -- HTTP server
  bracket (openLocalState initialUploadDBState)
          (createCheckpointAndClose)
          (\acid ->
            simpleHTTP httpConf (mainRoute acid)) --httpsForward

  -- HTTPS server
  --simpleHTTPS httpsConf mainHttp

mainHttp :: AcidState UploadDB -> ServerPart Response
mainHttp acid = do
  _ <- compressedResponseFilter
  mainRoute acid

httpsForward :: ServerPart Response
httpsForward = withHost $ \h -> uriRest $ \r -> do

  let url = "https://" ++ takeWhile (':' /=) h ++ ":"
                       ++ show (tlsPort httpsConf)
                       ++ r
  seeOther url (toResponse $ "Forwarding to: " ++ url ++ "\n")


mainRoute :: AcidState UploadDB -> ServerPart Response
mainRoute acid =
  do decodeBody myPolicy
     msum [ indexPart acid
          , do -- the "/" index page
            nullDir
            indexMostRecent acid

          , do -- to view download info
            dir "f" $ path $ \fileName -> fpart acid fileName

          , do -- to download:
            dir "s" $ path $ \fileName -> spart acid fileName

          , do -- server files from "/static/"
            dir "static" $ serveDirectory DisableBrowsing [] "static"

          , do -- anything else could not be found
            notFound $ toResponse Error.notFound
          ]

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" (10*10^(6 :: Int)) 1000 1000)

indexMostRecent :: AcidState UploadDB -> ServerPart Response
indexMostRecent acid = do
  now <- liftIO $ getCurrentTime
  mostRecent <- query' acid (MostRecentUploads now)
  ok $ toResponse $ baseHtml $ do
    index $ mapM_ recentFile mostRecent

indexPart :: AcidState UploadDB -> ServerPart Response
indexPart acid =
  do method [GET, POST]
     u <- lookFile "fileUpload"
     let uName = getFileName u
     let uPath = getFilePath u
     newName <- liftIO $ moveToRandomFile uploadDir 11 uPath
     let vName = uName
     let vPath = newName
     let vSName = drop (length uploadDir) vPath
     --TODO: extract content type and make it part of acid

     --acid stuff here
     --create new
     t <- liftIO $ getCurrentTime

     file <- update' acid (NewUpload t)

     let fID = file ^. fileID

     --edit the newly created file upload
     mFile <- query' acid (FileByID fID)

     case mFile of
       (Just f@(FileUpload{..})) -> msum
         [ do method POST
              let updatedFile = f & fpath        .~ vPath
                                  & fname        .~ vName
                                  & sfname       .~ vSName
                                  & uploadTime   .~ t
              _ <- update' acid (UpdateUpload updatedFile)

              ok $ toResponse $ upload updatedFile
         ]
       _ -> mzero -- FIXME

getFilePath :: (FilePath, FilePath, ContentType) -> FilePath
getFilePath (fp, _, _) = fp

getFileName :: (FilePath, FilePath, ContentType) -> FilePath
getFileName (_, name, _) = name

--getFileContents :: (FilePath, FilePath, ContentType) -> FilePath
--getFileContents (_, _, contents) =

fpart :: AcidState UploadDB -> String -> ServerPart Response
fpart acid s = do
  file <- query' acid (FileBySName s)
  case file of
    (Just file) -> do
      ok $ toResponse $ viewDownload file
    _ -> mzero

--serve the file response to dir "s"
spart :: AcidState UploadDB -> String -> ServerPart Response
spart acid s = do
  dlfile <- query' acid (FileBySName s)
  case dlfile of
    (Just dlfile) -> do
      let filepath = dlfile ^. fpath
      let trueName = dlfile ^. fname
      response <- serveFile (guessContentTypeM mimeTypes) filepath
      return $ setHeader "Content-Disposition"
        ("attachment; filename=\"" ++ trueName ++ "\"")
        response
    _ -> mzero --FIXME

