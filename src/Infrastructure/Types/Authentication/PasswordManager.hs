module Infrastructure.Types.Authentication.PasswordManager
  ( PasswordManager (..)
  , PasswordManagerError (..)
  ) where

import Core.Types.Authentication.Credentials     (Credentials, Password)
import Core.Types.EncryptedPassword              (EncryptedPassword)
import Core.Types.Id                             (Id)
import Core.Types.User                           (User)

import Crypto.JWT                                (Error)

import Infrastructure.Types.Authentication.Token (Token)

-- |
-- A 'PasswordManager' is the service dedicated at dealing with password and authentication tokens
-- It is indexed by a context 'm' which wraps the results.
data PasswordManager m = PasswordManager
  { generatePassword :: Credentials -> m EncryptedPassword
  -- ^ given some 'Credentials', tries to encrypt the password
  , generateToken    :: Id User -> m Token
  -- ^ given a 'User' 'Id', tries to generate an authentication 'Token'
  , validatePassword :: User -> Password -> Bool
  -- ^ given a 'User' and a non excrypted 'Password', checks whether the password corresponds to the user's one
  }

-- |
-- How the 'PasswordManager' operations can fail
data PasswordManagerError
  = -- | there was an error while hashing the password
    FailedHashing
  | -- | there was an error while generating the authentication token
    FailedJWTCreation Error
  deriving stock (Show)
