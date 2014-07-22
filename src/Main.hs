module Main where

import Control.Concurrent
import Control.Monad

import Happstack.Server
import Happstack.Server.SimpleHTTPS
import Happstack.Server.Compression

import HTML.Index
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
  _ <- forkIO $ simpleHTTP httpConf httpsForward

  -- HTTPS server
  simpleHTTPS httpsConf mainHttp

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
mainRoute = msum
  [ do -- the "/" index page
       nullDir
       ok $ toResponse index

  , do -- server files from "/static/"
       dir "static" $ serveDirectory DisableBrowsing [] "static"

  , do -- anything else could not be found
       notFound $ toResponse Error.notFound
  ]
