{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module UserUtils where

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens.Operators

import Crypto.Scrypt

import Data.Acid
import Data.Acid.Advanced ( query', update' )

import Happstack.Server
import Happstack.Server.SimpleHTTPS
import Happstack.Server.Compression
import Happstack.Server.ClientSession

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Bindrop.State
import Bindrop.State.UploadDB
import Bindrop.State.Users
import Session
import HTML.Index
import HTML.File
import HTML.Login
import HTML.Register
import HTML.Upload

mkEncrypted :: B.ByteString -> IO EncryptedPass
mkEncrypted pw = encryptPassIO defaultParams (Pass pw)

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
           then ok $ toResponse $ loginSuccessful (Just mUser)
           else do
             putSession $ SessionData Nothing
             ok $ toResponse $ loginFailed Nothing

       _ -> ok $ toResponse $ loginFailed Nothing

isUniqueUser :: Maybe User -> Bool
isUniqueUser user =
  case user of
    (Just user) -> False
    Nothing     -> True

logoutPart :: ClientSessionT SessionData (ServerPartT IO) Response
logoutPart = do
  expireSession
  ok $ toResponse $ logout

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

extractUser :: Maybe User -> User
extractUser u =
  case u of
    (Just u) -> u
    Nothing  -> User 0 "" "" (C.pack "") 0

