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
uRegisterPart acid = do
  method [GET, POST]
  userName    <- look "username"
  userEmail   <- look "email"
  userPhrase  <- look "phrase"
  answerInput <- look "answer"
  passInput   <- look "pass"
  cPassInput  <- look "cpass"
  userPass    <- liftIO $ mkEncrypted $ C.pack passInput
  userAnswer  <- liftIO $ mkEncrypted $ C.pack answerInput

  -- test that passwords are matching
  let pMatch = passInput == cPassInput

  -- only accept unique usernames and emails
  mUserByName <- query' acid (UserByName userName)
  mUserByEmail <- query' acid (UserByEmail userEmail)
  let areUnique = if (isUniqueUser mUserByName && isUniqueUser mUserByEmail)
                    then True
                    else False
  if pMatch && areUnique
    then do
      -- create new user
      user <- update' acid NewUser
      let uID = user ^. userID

      --edit the newly created user
      mUser <- query' acid (UserByID uID)

      case mUser of
        (Just mUser) -> do
          method POST
          let updatedUser = mUser & uName   .~ userName
                                  & uEmail  .~ userEmail
                                  & uPass   .~ (getEncryptedPass userPass)
                                  & uPhrase .~ userPhrase
                                  & uAnswer .~ (getEncryptedPass userAnswer)
                                  & count   .~ 0
          _ <- update' acid (UpdateUser updatedUser)
          _ <- update' acid UpdateACount
          ok $ toResponse $ registrationSuccess updatedUser

        _ -> ok $ toResponse $ registrationFail

    else ok $ toResponse $ registrationFail

loginPart :: AcidState BindropState ->
  ClientSessionT SessionData (ServerPartT IO) Response
loginPart acid = do
  method [GET, POST]
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
      let updatedUser = u & userID  .~ u ^. userID
                          & uName   .~ u ^. uName
                          & uEmail  .~ u ^. uEmail
                          & uPass   .~ u ^. uPass
                          & uPhrase .~ u ^. uPhrase
                          & uAnswer .~ u ^. uAnswer
                          & count   %~ succ
      _ <- update' acid (UpdateUser updatedUser)

      -- update cookie
      putSession $ SessionData $ Just updatedUser

      ok $ toResponse $ upload mU f
    _ -> mzero --FIXME

changePassPart
  :: AcidState BindropState
  -> Maybe User
  -> ClientSessionT SessionData (ServerPartT IO) Response
changePassPart acid u = do
  method POST
  oldPass  <- look "oldPass"
  newPass  <- look "newPass"
  cNewPass <- look "cNewPass"

  case u of
    (Just u) -> do
      let oldUserPass    = Pass $ C.pack oldPass
      let newPassesMatch = newPass == cNewPass
      let match = verifyPass' oldUserPass $ EncryptedPass (u ^. uPass)
      newUserPass <- liftIO $ mkEncrypted $ C.pack newPass

      if match && newPassesMatch
        then do
          let updatedUser = u & userID  .~ u ^. userID
                              & uName   .~ u ^. uName
                              & uEmail  .~ u ^. uEmail
                              & uPass   .~ (getEncryptedPass newUserPass)
                              & uPhrase .~ u ^. uPhrase
                              & uAnswer .~ u ^. uAnswer
                              & count   .~ u ^. count
          _ <- update' acid (UpdateUser updatedUser)

          -- update cookie
          putSession $ SessionData $ Just updatedUser

          ok $ toResponse $ changePassSuccess (Just updatedUser)
        else ok $ toResponse $ changePassFail (Just u)

    Nothing -> ok $ toResponse $ changePassFail Nothing

lostPassPart
  :: AcidState BindropState
  -> Maybe User
  -> ClientSessionT SessionData (ServerPartT IO) Response
lostPassPart acid u = do
  case u of
    (Just u) -> ok $ toResponse $ lostPassReset (Just u) ""

    Nothing -> do
      username <- look "username"
      user <- query' acid (UserByName username)
      case user of
        (Just user) -> do
          let phrase = user ^. uPhrase
          ok $ toResponse $ lostPassReset u phrase

        Nothing -> ok $ toResponse $ lostPassFail Nothing

resetPassPart
  :: AcidState BindropState
  -> Maybe User
  -> ClientSessionT SessionData (ServerPartT IO) Response
resetPassPart acid u = do
  case u of
    (Just u) -> ok $ toResponse $ lostPassFail Nothing
    Nothing  -> do
      email  <- look "email"
      answer <- look "answer"
      passInput  <- look "pass"
      cPassInput <- look "cpass"
      let answerInput = Pass $ C.pack answer
      user   <- query' acid (UserByEmail email)
      case user of
        (Just user) -> do
          let passMatch = passInput == cPassInput
          let match = verifyPass' answerInput $ EncryptedPass (user ^. uAnswer)
          if passMatch && match
            then do
              newUserPass <- liftIO $ mkEncrypted $ C.pack passInput
              let updatedUser = user & userID  .~ user ^. userID
                                     & uName   .~ user ^. uName
                                     & uEmail  .~ user ^. uEmail
                                     & uPass   .~ (getEncryptedPass newUserPass)
                                     & uPhrase .~ user ^. uPhrase
                                     & uAnswer .~ user ^. uAnswer
                                     & count   .~ user ^. count
              _ <- update' acid (UpdateUser updatedUser)
              ok $ toResponse $ lostPassSuccess Nothing

            else ok $ toResponse $ lostPassFail Nothing

        Nothing -> ok $ toResponse $ lostPassFail Nothing

