module UserUtils where

import Crypto.Scrypt
import qualified Data.ByteString as B

mkEncrypted :: B.ByteString -> IO EncryptedPass
mkEncrypted pw = encryptPassIO defaultParams (Pass pw)

{- todo
uRegisterPart :: AcidState Users -> user? -> ServerPart Response
uRegisterPart uAcid user =
-}

