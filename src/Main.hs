module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception (bracket)

import Data.Acid
import Data.Acid.Local (createCheckpointAndClose)

import Happstack.Server
import Happstack.Server.SimpleHTTPS
import Happstack.Server.Compression

import System.Directory (renameFile)

import HTML.Index
import HTML.Upload
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
uploadDir = "/home/kvitebjorn/Documents/bindrop/files/"

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
     msum [ indexPart
          , do -- the "/" index page
            nullDir
            ok $ toResponse index

          , do -- server files from "/static/"
            dir "static" $ serveDirectory DisableBrowsing [] "static"

          , do -- anything else could not be found
            notFound $ toResponse Error.notFound
          ]

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" (10*10^6) 1000 1000)

indexPart :: ServerPart Response
indexPart =
  do method [GET, POST]
     u <- lookFile "fileUpload"
     liftIO $ renameFile (tmpFilePath u) (uploadDir ++
       tmpFileName u)
     let v = updateFileInfo u
     --put acid stuff here
     --TODO: extract content type and make it part of acid
     ok $ toResponse $ upload v

tmpFilePath :: (FilePath, FilePath, ContentType) -> FilePath
tmpFilePath (fp, _, _) = fp

tmpFileName :: (FilePath, FilePath, ContentType) -> FilePath
tmpFileName (_, name, _) = name

updateFileInfo :: (FilePath, FilePath, ContentType) -> (FilePath, FilePath, ContentType)
updateFileInfo (tmpPath, name, contentType) =
  ((uploadDir ++ name)
  , name
  , contentType)

