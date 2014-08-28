module Bindrop.Responses.IndexResponses where

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens.Operators

import Data.Acid
import Data.Acid.Advanced ( query' )
import Data.Time          ( getCurrentTime )

import Happstack.Server
import Happstack.Server.ClientSession

import Bindrop.Session
import Bindrop.State
import Bindrop.State.UploadDB
import Bindrop.State.Users
import Bindrop.Responses.FileResponses

import HTML.Base
import HTML.File
import HTML.Index

import Utils.FileUtils

indexMostRecent
  :: AcidState BindropState
  -> Maybe User
  -> ClientSessionT SessionData (ServerPartT IO) Response
indexMostRecent acid u = do
  now <- liftIO $ getCurrentTime
  mostRecent <- query' acid (MostRecentUploads now)
  ok $ toResponse $ index u $ mapM_ recentUpload mostRecent

indexPart :: Maybe User -> AcidState BindropState ->
  ClientSessionT SessionData (ServerPartT IO) Response
indexPart mU acid = do
  method [GET, POST]
  u <- lookFile "fileUpload"
  let uName = getFileName u
  case uName of
    "" -> indexMostRecent acid mU  --no file was selected on the form
    _  -> handleFile mU acid u

