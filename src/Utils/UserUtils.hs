module Utils.UserUtils where

import Crypto.Scrypt

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Bindrop.State.Users

mkEncrypted :: B.ByteString -> IO EncryptedPass
mkEncrypted pw = encryptPassIO defaultParams (Pass pw)

isUniqueUser :: Maybe User -> Bool
isUniqueUser user =
  case user of
    (Just user) -> False
    Nothing     -> True

extractUser :: Maybe User -> User
extractUser u =
  case u of
    (Just u) -> u
    Nothing  -> User 0 "" "" (C.pack "") 0

