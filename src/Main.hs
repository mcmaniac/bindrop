module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception  ( bracket )

import Data.Acid
import Data.Acid.Local    ( createCheckpointAndClose )

import Happstack.Server
import Happstack.Server.Compression
import Happstack.Server.ClientSession
import Happstack.Server.SimpleHTTPS

import Bindrop.Session
import Bindrop.State
import Bindrop.Routes ( mainRoute )

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
  key <- getDefaultKey
  let sessionConf = mkSessionConf key

  -- HTTP server
  bracket (openLocalState initialBindropState)
          (createCheckpointAndClose)
          (\acid -> simpleHTTP httpConf $
            withClientSessionT sessionConf $ mainRoute acid)

  -- HTTPS server
  --simpleHTTPS httpsConf mainHttp

mainHttp :: AcidState BindropState
  -> ClientSessionT SessionData (ServerPartT IO) Response
mainHttp acid = do
  _ <- compressedResponseFilter
  mainRoute acid

httpsForward :: ServerPart Response
httpsForward = withHost $ \h -> uriRest $ \r -> do

  let url = "https://" ++ takeWhile (':' /=) h ++ ":"
                       ++ show (tlsPort httpsConf)
                       ++ r
  seeOther url (toResponse $ "Forwarding to: " ++ url ++ "\n")

