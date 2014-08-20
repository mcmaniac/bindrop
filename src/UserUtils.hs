module UserUtils where

import Crypto.Scrypt
import qualified Data.ByteString as B

mkEncrypted :: ByteString -> IO EncryptedPass
mkEncrypted pw = encryptPassIO defaultParams (Pass pw)

