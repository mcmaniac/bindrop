module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import Happstack.Server
import Happstack.Server.SimpleHTTPS
import Happstack.Server.Compression

import System.Directory (renameFile)

import HTML.Index
import HTML.Upload
import qualified HTML.Error as Error

httpConf :: Conf
httpConf = nullConf { port = 8082 }

httpsConf :: TLSConf
httpsConf = nullTLSConf
  { tlsPort = 8083
  , tlsCert = "nils.cc.crt"
  , tlsKey  = "nils.key"
  }

main :: IO ()
main = do
  putStrLn "Starting server..."

  -- HTTP server
  simpleHTTP httpConf mainRoute --httpsForward

  -- HTTPS server
  --simpleHTTPS httpsConf mainHttp

mainHttp :: ServerPart Response
mainHttp = do
  _ <- compressedResponseFilter
  mainRoute

httpsForward :: ServerPart Response
httpsForward = withHost $ \h -> uriRest $ \r -> do

  let url = "https://" ++ takeWhile (':' /=) h ++ ":"
                       ++ show (tlsPort httpsConf)
                       ++ r

  seeOther url (toResponse $ "Forwarding to: " ++ url ++ "\n")

mainRoute :: ServerPart Response
mainRoute =
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
     liftIO $ renameFile (tmpFilePath u) ("/tmp/bindrop/" ++
       tmpFileName u)
     let v = updateFileInfo u
     ok $ toResponse $ upload v

tmpFilePath :: (FilePath, FilePath, ContentType) -> FilePath
tmpFilePath (fp, _, _) = fp

tmpFileName :: (FilePath, FilePath, ContentType) -> FilePath
tmpFileName (_, name, _) = name

updateFileInfo :: (FilePath, FilePath, ContentType) -> (FilePath, FilePath, ContentType)
updateFileInfo (tmpPath, name, contentType) =
  (("/tmp/bindrop/" ++ name)
  , name
  , contentType)

