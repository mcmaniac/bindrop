module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception  ( bracket )
import Control.Lens.Iso   ( non )
import Control.Lens.Operators

import Data.Acid
import Data.Acid.Local    ( createCheckpointAndClose )
import Data.Acid.Advanced ( query', update' )
import qualified Data.ByteString.Char8 as B
import Data.Time          ( getCurrentTime )

import Happstack.Server
import Happstack.Server.SimpleHTTPS
import Happstack.Server.Compression
import Happstack.Server.ClientSession

import HTML.About
import HTML.Base
import HTML.Index
import HTML.File
import HTML.Upload
import HTML.Download
import HTML.Register
import HTML.Login
import Session
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


mainRoute :: AcidState BindropState
  -> ClientSessionT SessionData (ServerPartT IO) Response
mainRoute acid = do
  s <- getSession
  let mU = s ^. sessionUser

  do decodeBody myPolicy
     msum [ indexPart mU acid -- update index with file uploads

          , do -- the "/" index page
            nullDir
            indexMostRecent acid

          , do -- to view download info
            dir "f" $ path $ \fileName -> fpart mU acid fileName

          , do -- to download:
            dir "s" $ path $ \fileName -> spart acid fileName

          , do -- login
            dirs "u/login" $ loginPart acid

          , do -- logout
            dirs "u/logout" $ logoutPart mU

          , do -- process registration
            dirs "u/r/process" $ uRegisterPart acid

          , do -- user registration page
            dirs "u/r" $ ok $ toResponse $ register mU

          , do -- user specific uploads
            dirs "u/m/uploads" $ myUploadsPart acid mU

          , do -- make a file private via button
            dirs "u/m/privacy" $ path $ \fileName -> makePrivatePart acid fileName --lmao private part

          , do -- my account page
            dirs "u/m" $ myAcctPart mU

          , do -- user login page
            dir "u" $ ok $ toResponse $ loginPage mU

          , do -- about page
            dir "a" $ ok $ toResponse $ about mU

          , do -- server files from "/static/"
            dir "static" $ serveDirectory DisableBrowsing [] "static"

          , do -- anything else could not be found
            notFound $ toResponse Error.notFound
          ]

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" (10*10^(6 :: Int)) 1000 1000)

indexMostRecent :: AcidState BindropState ->
  ClientSessionT SessionData (ServerPartT IO) Response
indexMostRecent acid = do
  now <- liftIO $ getCurrentTime
  mostRecent <- query' acid (MostRecentUploads now)
  --get user info
  s <- getSession
  let mU = s ^. sessionUser
  ok $ toResponse $ baseHtml $ do
    index mU $ mapM_ recentUpload mostRecent

indexPart :: Maybe User -> AcidState BindropState ->
  ClientSessionT SessionData (ServerPartT IO) Response
indexPart mU acid =
  do method [GET, POST]
     u <- lookFile "fileUpload"

     let uName = getFileName u

     case uName of
       "" -> indexMostRecent acid  --no file was selected on the form
       _  -> handleFile mU acid u

handleFile
  :: Maybe User
  -> AcidState BindropState
  -> (FilePath, FilePath, ContentType)
  -> ClientSessionT SessionData (ServerPartT IO) Response
handleFile mU acid u = do
  let uname = getFileName u
  let uPath = getFilePath u
  newName <- liftIO $ moveToRandomFile uploadDir 11 uPath
  let vName = uname
  let vPath = newName
  let vSName = drop (length uploadDir) vPath

  --uploader's user name
  let username = UserName $ (extractUser mU) ^. uName

  --acid stuff here
  --create new file
  t <- liftIO $ getCurrentTime
  file <- update' acid (NewUpload t)
  let fID = file ^. fileID

  --edit the newly created file upload
  mFile <- query' acid (FileByID fID)

  case mFile of
    (Just mFile) -> do
      method POST
      let updatedFile = mFile & fpath        .~ vPath
                              & fname        .~ vName
                              & sfname       .~ vSName
                              & uploadTime   .~ t
                              & userName     .~ username

      _ <- update' acid (UpdateUpload updatedFile)

      updateUserCount acid updatedFile mU

    _ -> mzero -- FIXME

updateUserCount :: AcidState BindropState
  -> FileUpload
  -> Maybe User
  -> ClientSessionT SessionData (ServerPartT IO) Response
updateUserCount acid f u = do
  let mU = u
  case u of
    (Just u) -> do
      method POST
      let updatedUser = u & userID .~ u ^. userID
                          & uName  .~ u ^. uName
                          & uEmail .~ u ^. uEmail
                          & uPass  .~ u ^. uPass
                          & count  %~ succ
      _ <- update' acid (UpdateUser updatedUser)

      -- update cookie
      putSession $ SessionData $ Just updatedUser

      ok $ toResponse $ upload mU f
    _ -> mzero --FIXME

getFilePath :: (FilePath, FilePath, ContentType) -> FilePath
getFilePath (fp, _, _) = fp

getFileName :: (FilePath, FilePath, ContentType) -> FilePath
getFileName (_, name, _) = name

--getFileContents :: (FilePath, FilePath, ContentType) -> FilePath
--getFileContents (_, _, contents) =

fpart :: Maybe User -> AcidState BindropState -> String ->
  ClientSessionT SessionData (ServerPartT IO) Response
fpart u acid s = do
  file <- query' acid (FileBySName s)
  case file of
    (Just file) -> do
      ok $ toResponse $ viewDownload u file
    _ -> mzero

--serve the file response to dir "s"
spart :: AcidState BindropState -> String ->
  ClientSessionT SessionData (ServerPartT IO) Response
spart acid s = do
  dlfile <- query' acid (FileBySName s)
  case dlfile of
    (Just dlfile) -> do
      let filepath = dlfile ^. fpath
      let trueName = dlfile ^. fname

      --update file download count
      let updatedFile = dlfile & fileID      .~ dlfile ^. fileID
                               & fname       .~ dlfile ^. fname
                               & sfname      .~ dlfile ^. sfname
                               & uploadTime  .~ dlfile ^. uploadTime
                               & public      .~ dlfile ^. public
                               & userName    .~ dlfile ^. userName
                               & dlCount     %~ succ
      _ <- update' acid (UpdateUpload updatedFile)

      response <- serveFile (guessContentTypeM mimeTypes) filepath
      return $ setHeader "Content-Disposition"
        ("attachment; filename=\"" ++ trueName ++ "\"")
        response

    _ -> mzero --FIXME

makePrivatePart :: AcidState BindropState
  -> String
  -> ClientSessionT SessionData (ServerPartT IO) Response
makePrivatePart acid fName = do
  f <- query' acid (FileBySName fName)
  case f of
    (Just f) -> do method POST
                   let currentPrivacy = f ^. public
                   let updatedFile = f & public .~ not currentPrivacy
                   s <- getSession
                   let mU = s ^. sessionUser

                   _ <- update' acid (UpdateUpload updatedFile)

                   ok $ toResponse $ makePrivate updatedFile mU

    _ -> mzero -- FIXME

