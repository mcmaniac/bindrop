module Bindrop.Responses.FileResponses where

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens.Operators

import Data.Acid
import Data.Acid.Advanced ( query', update' )
import Data.Time          ( getCurrentTime )
import qualified Data.ByteString.Char8 as B

import Happstack.Server
import Happstack.Server.ClientSession

import Bindrop.Session
import Bindrop.State
import Bindrop.State.UploadDB
import Bindrop.State.Users
import Bindrop.Responses.UserResponses

import HTML.File      ( makePrivate  )
import HTML.Download  ( viewDownload )

import Utils.FileUtils
import Utils.UserUtils

uploadDir :: FilePath
uploadDir = "files/"

fpart
  :: AcidState BindropState
  -> Maybe User
  -> String
  -> ClientSessionT SessionData (ServerPartT IO) Response
fpart acid u s = do
  file <- query' acid (FileBySName s)
  case file of
    (Just file) -> do
      ok $ toResponse $ viewDownload u file
    _ -> mzero

--serve the file response to dir "s"
spart
  :: AcidState BindropState
  -> String
  -> ClientSessionT SessionData (ServerPartT IO) Response
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

makePrivatePart
  :: AcidState BindropState
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

