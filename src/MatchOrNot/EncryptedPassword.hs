module MatchOrNot.EncryptedPassword (EncryptedPassword, asBytestring, encryptPassword, validatePassword) where

import Crypto.BCrypt (fastBcryptHashingPolicy, hashPasswordUsingPolicy)
import Crypto.BCrypt qualified as BCrypt (validatePassword)
import Data.ByteString (ByteString)
import MatchOrNot.Types.EncryptedPassword (EncryptedPassword (EncryptedPassword, asBytestring))

-- |
-- encrypt a 'ByteString' into an 'EncryptedPassword' using bcrypt with 'fastBcryptHashingPolicy'
encryptPassword :: ByteString -> IO (Maybe EncryptedPassword)
encryptPassword password = fmap EncryptedPassword <$> hashPasswordUsingPolicy fastBcryptHashingPolicy password

-- |
-- Given an 'EncryptedPassword' and a 'ByteString' password, it checks whether the password is valid
validatePassword :: EncryptedPassword -> ByteString -> Bool
validatePassword (EncryptedPassword password) = BCrypt.validatePassword password
