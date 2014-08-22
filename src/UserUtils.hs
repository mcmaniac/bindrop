{-# LANGUAGE RecordWildCards #-}

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

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Bindrop.State
import Bindrop.State.Users
import HTML.Index
import HTML.Login
import HTML.Register

mkEncrypted :: B.ByteString -> IO EncryptedPass
mkEncrypted pw = encryptPassIO defaultParams (Pass pw)

uRegisterPart :: AcidState BindropState -> ServerPart Response
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
                       == True
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

loginPart :: AcidState BindropState -> ServerPart Response
loginPart acid =
  do method [GET, POST]
     userName <- look "username"
     passInput <- look "pass"

     mUser <- query' acid (UserByName userName)

     -- verify the password
     case mUser of
       (Just mUser) -> do
         let userPass = Pass $ C.pack passInput
         let match    = verifyPass' userPass $ EncryptedPass (mUser ^. uPass)

         if match == True
           then ok $ toResponse $ loginSuccessful userName
         else
           ok $ toResponse $ loginFailed

       _ -> mzero

isUniqueUser :: Maybe User -> Bool
isUniqueUser user =
  case user of
    (Just user) -> False
    Nothing     -> True

