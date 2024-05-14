module Infrastructure.Authentication.PasswordManager
  ( hoist
  , bcryptPasswordManager
  ) where

import           Control.Category                                    ((>>>))
import           Control.Monad.Trans.Except                          (ExceptT (ExceptT))

import           Data.Bifunctor                                      (bimap)

import           Infrastructure.Types.Authentication.PasswordManager (PasswordManager (..),
                                                                      PasswordManagerError (..))
import           Infrastructure.Types.Authentication.Token           (Token (Token))

import           MatchOrNot.Authentication.Credentials               (Credentials,
                                                                      Password (asBytestring))
import qualified MatchOrNot.Authentication.Credentials               as Credentials (password)
import           MatchOrNot.EncryptedPassword                        (encryptPassword)
import qualified MatchOrNot.EncryptedPassword                        as Encrypted (validatePassword)
import           MatchOrNot.Types.EncryptedPassword                  (EncryptedPassword)
import           MatchOrNot.Types.Id                                 (Id)
import           MatchOrNot.Types.User                               (User (password))

import           Servant.Auth.Server                                 (JWTSettings, makeJWT)

-- |
-- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'PasswordManager' is operating
hoist :: (forall a. m a -> n a) -> PasswordManager m -> PasswordManager n
hoist f PasswordManager{generatePassword, generateToken, validatePassword} =
  PasswordManager (f . generatePassword) (f . generateToken) validatePassword

-- |
-- A 'PasswordManager' implementation based on the 'bcrypt' algorithm
bcryptPasswordManager
  :: JWTSettings -> PasswordManager (ExceptT PasswordManagerError IO)
bcryptPasswordManager jwtSettings =
  PasswordManager
    { generatePassword = bcryptGeneratePassword
    , generateToken = bcryptGenerateToken jwtSettings
    , validatePassword = bcryptValidatePassword
    }

bcryptGeneratePassword
  :: Credentials -> ExceptT PasswordManagerError IO EncryptedPassword
bcryptGeneratePassword =
  -- extract the password from the Credentials
  Credentials.password
    -- convert it to bytestring
    >>> asBytestring
    -- try to encrypt it
    >>> encryptPassword
    -- wrap the error message to get a PasswordManagerError
    >>> fmap (maybe (Left FailedHashing) Right)
    -- wrap everything in ExceptT
    >>> ExceptT

bcryptGenerateToken :: JWTSettings -> Id User -> ExceptT PasswordManagerError IO Token
bcryptGenerateToken jwtSettings userId = ExceptT $ do
  -- try to generate the token containing the userId
  -- the Nothing means that the token does not expire
  token <- makeJWT userId jwtSettings Nothing
  -- wrap the error to get a PasswordErrorManager and the token to get a Token
  pure $ bimap FailedJWTCreation Token token

bcryptValidatePassword :: User -> Password -> Bool
bcryptValidatePassword user password' = Encrypted.validatePassword (password user) (asBytestring password')
