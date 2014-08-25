{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, RecordWildCards #-}

module UserUtils where

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens.Operators
import Control.Lens.TH ( makeLenses )

import Crypto.Scrypt

import Data.Acid
import Data.Acid.Advanced ( query', update' )

import Happstack.Server
import Happstack.Server.SimpleHTTPS
import Happstack.Server.Compression
import Happstack.Server.ClientSession

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Data ( Data, Typeable )
import Data.SafeCopy ( base, deriveSafeCopy )

import Bindrop.State
import Bindrop.State.UploadDB
import Bindrop.State.Users
import HTML.Index
import HTML.File
import HTML.Login
import HTML.Register
import HTML.Upload

--cookie
data SessionData = SessionData
  { _user :: Maybe User
  } deriving (Eq, Ord, Show, Data, Typeable)

$(makeLenses ''SessionData)
$(deriveSafeCopy 0 'base ''SessionData)

instance ClientSession SessionData where
  emptySession = SessionData { _user = Nothing }

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
           (Just u@(User{..})) -> msum
             [ do method POST
                  let updatedUser = u & uName  .~ userName
                                      & uEmail .~ userEmail
                                      & uPass  .~ (getEncryptedPass userPass)
                  _ <- update' acid (UpdateUser updatedUser)
                  ok $ toResponse $ registrationSuccess updatedUser
             ]
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
           then do
             u <- getSession
             let mU = u ^. user
             ok $ toResponse $ loginSuccessful mU
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
      s <- getSession --convenient Maybe User
      let mU = s ^. user
      ok $ toResponse $ do --baseHtml $ do
        myUploads mU $ mapM_ uploadedFile userUploads

    Nothing -> mzero

