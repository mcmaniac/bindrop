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

import HTML.About
import HTML.Base
import HTML.Index
import HTML.File
import HTML.Upload
import HTML.Download
import HTML.Register
import FileUtils
import UserUtils
import Bindrop.State
import Bindrop.State.UploadDB
import Bindrop.State.Users
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
  bracket (openLocalState initialBindropState)
          (createCheckpointAndClose)
          (\acid -> simpleHTTP httpConf (mainRoute acid))

  -- HTTPS server
  --simpleHTTPS httpsConf mainHttp

mainHttp :: AcidState BindropState -> ServerPart Response
mainHttp acid = do
  _ <- compressedResponseFilter
  mainRoute acid

httpsForward :: ServerPart Response
httpsForward = withHost $ \h -> uriRest $ \r -> do

  let url = "https://" ++ takeWhile (':' /=) h ++ ":"
                       ++ show (tlsPort httpsConf)
                       ++ r
  seeOther url (toResponse $ "Forwarding to: " ++ url ++ "\n")


mainRoute :: AcidState BindropState -> ServerPart Response
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

          --, do -- user login page
            --dir "u" $ ok $ toResponse $ login

          , do -- user registration page
            dir "r" $ ok $ toResponse $ register

          , do -- process registration
            dirs "u/r" $ uRegisterPart acid

          , do -- about page
            dir "a" $ ok $ toResponse $ about

          , do -- server files from "/static/"
            dir "static" $ serveDirectory DisableBrowsing [] "static"

          , do -- anything else could not be found
            notFound $ toResponse Error.notFound
          ]

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" (10*10^(6 :: Int)) 1000 1000)

indexMostRecent :: AcidState BindropState -> ServerPart Response
indexMostRecent acid = do
  now <- liftIO $ getCurrentTime
  mostRecent <- query' acid (MostRecentUploads now)
  ok $ toResponse $ baseHtml $ do
    index $ mapM_ recentFile mostRecent

indexPart :: AcidState BindropState -> ServerPart Response
indexPart acid =
  do method [GET, POST]
     u <- lookFile "fileUpload"
     let uName = getFileName u

     case uName of
       "" -> indexMostRecent acid  --no file was selected on the form
       _  -> handleFile acid u

handleFile
  :: AcidState BindropState
  -> (FilePath, FilePath, ContentType)
  -> ServerPart Response
handleFile acid u = do
         let uName = getFileName u
         let uPath = getFilePath u
         newName <- liftIO $ moveToRandomFile uploadDir 11 uPath
         let vName = uName
         let vPath = newName
         let vSName = drop (length uploadDir) vPath

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

fpart :: AcidState BindropState -> String -> ServerPart Response
fpart acid s = do
  file <- query' acid (FileBySName s)
  case file of
    (Just file) -> do
      ok $ toResponse $ viewDownload file
    _ -> mzero

--serve the file response to dir "s"
spart :: AcidState BindropState -> String -> ServerPart Response
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

