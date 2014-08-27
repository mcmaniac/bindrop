{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Bindrop.Responses.UserResponses where

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens.Operators

import Crypto.Scrypt

import Data.Acid
import Data.Acid.Advanced ( query', update' )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Happstack.Server
import Happstack.Server.ClientSession

import Bindrop.Session
import Bindrop.State
import Bindrop.State.UploadDB
import Bindrop.State.Users

import HTML.Index
import HTML.File
import HTML.Login
import HTML.Register
import HTML.Upload

import Utils.UserUtils

uRegisterPart :: AcidState BindropState ->
  ClientSessionT SessionData (ServerPartT IO) Response
uRegisterPart acid =
  do method [GET, POST]
     userName  <- look "username"
     userEmail <- look "email"
     passInput <- look "pass"
     userPass  <- liftIO $ mkEncrypted $ C.pack passInput

     -- only accept unique usernames and emails
     mUserByName <- query' acid (UserByName userName)
     mUserByEmail <- query' acid (UserByEmail userEmail)
     let areUnique = if (isUniqueUser mUserByName && isUniqueUser mUserByEmail)
                       then True
                     else False
     case areUnique of
       True -> do
         -- create new user
         user <- update' acid NewUser
         let uID = user ^. userID

         --edit the newly created user
         mUser <- query' acid (UserByID uID)

         case mUser of
           (Just mUser) -> do
             method POST
             let updatedUser = mUser & uName  .~ userName
                                     & uEmail .~ userEmail
                                     & uPass  .~ (getEncryptedPass userPass)
                                     & count  .~ 0
             _ <- update' acid (UpdateUser updatedUser)
             ok $ toResponse $ registrationSuccess updatedUser

           _ -> ok $ toResponse $ registrationFail

       False -> ok $ toResponse $ registrationFail

loginPart :: AcidState BindropState ->
  ClientSessionT SessionData (ServerPartT IO) Response
loginPart acid =
  do method [GET, POST]
     userName <- look "username"
     passInput <- look "pass"

     mUser <- query' acid (UserByName userName)
     putSession $ SessionData mUser

     -- verify the password
     case mUser of
       (Just mUser) -> do
         let userPass = Pass $ C.pack passInput
         let match    = verifyPass' userPass $ EncryptedPass (mUser ^. uPass)

         if match
           then ok $ toResponse $ login (Just mUser)
           else do
             putSession $ SessionData Nothing
             ok $ toResponse $ login Nothing

       _ -> ok $ toResponse $ login Nothing

logoutPart :: Maybe User -> ClientSessionT SessionData (ServerPartT IO) Response
logoutPart u = do
  expireSession
  ok $ toResponse $ logout u

myAcctPart :: Maybe User -> ClientSessionT SessionData (ServerPartT IO) Response
myAcctPart u = do
  ok $ toResponse $ myAcct u

myUploadsPart :: AcidState BindropState
  -> Maybe User
  -> ClientSessionT SessionData (ServerPartT IO) Response
myUploadsPart acid u = do
  case u of
    (Just u) -> do
      let username = UserName $ u ^. uName
      userUploads <- query' acid (UploadsByUserName username)
      ok $ toResponse $ do
        myUploads (Just u) $ mapM_ (uploadedFile u) userUploads

    Nothing -> mzero

updateUserCount
  :: AcidState BindropState
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

