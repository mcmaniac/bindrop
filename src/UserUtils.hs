{-# LANGUAGE RecordWildCards #-}

module UserUtils where

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens.Operators

import Data.Acid
import Data.Acid.Advanced ( query', update' )

import Happstack.Server
import Happstack.Server.SimpleHTTPS
import Happstack.Server.Compression

import Crypto.Scrypt
import qualified Data.ByteString as B

import Users
import HTML.Register

mkEncrypted :: B.ByteString -> IO EncryptedPass
mkEncrypted pw = encryptPassIO defaultParams (Pass pw)

uRegisterPart :: AcidState Users -> ServerPart Response
uRegisterPart uAcid = do
  userName <- look "username"
  userEmail <- look "email"
  userPass <- look "pass"
  user <- update' uAcid NewUser
  let uID = user ^. userID

  --edit the newly created file upload
  mUser <- query' uAcid (UserByID uID)

  case mUser of
    (Just u@(User{..})) -> msum
      [ do method POST
           let updatedUser = u & uName  .~ userName
                               & uEmail .~ userEmail
                               & uPass  .~ userPass
           _ <- update' uAcid (UpdateUser updatedUser)

           ok $ toResponse $ registrationSuccess updatedUser
      ]
    _ -> mzero -- FIXME

