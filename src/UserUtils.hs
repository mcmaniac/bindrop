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
import qualified Data.ByteString.Lazy as L

import Bindrop.State
import Bindrop.State.Users
import HTML.Register

mkEncrypted :: B.ByteString -> IO EncryptedPass
mkEncrypted pw = encryptPassIO defaultParams (Pass pw)

uRegisterPart :: AcidState BindropState -> ServerPart Response
uRegisterPart acid = do
  userName <- look "username"
  userEmail <- look "email"
  userPass <- fmap L.toStrict (lookBS "pass")

  user <- update' acid NewUser
  let uID = user ^. userID

  --edit the newly created file upload
  mUser <- query' acid (UserByID uID)

  case mUser of
    (Just u@(User{..})) -> msum
      [ do method POST
           let updatedUser = u & uName  .~ userName
                               & uEmail .~ userEmail
                               & uPass  .~ userPass
           _ <- update' acid (UpdateUser updatedUser)

           ok $ toResponse $ registrationSuccess updatedUser
      ]
    _ -> mzero -- FIXME

