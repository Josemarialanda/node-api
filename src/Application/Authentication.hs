module Application.Authentication
  ( authenticator
  ) where

import           Application.Types.Authentication                    (Error (..))
import           Application.Types.User.Error                        (UserRepositoryError)

import           Control.Monad.Trans.Except                          (ExceptT, throwE, withExceptT)

import           Core.Types.Authentication.Authenticator             (Authenticator (..))
import           Core.Types.Authentication.Credentials               (Credentials (..))
import           Core.Types.Id                                       (Id)
import           Core.Types.User                                     (User, UserRepository)
import qualified Core.Types.User                                     as UserRepo (findByName)

import           Infrastructure.Types.Authentication.PasswordManager (PasswordManager (validatePassword))

authenticator
  :: UserRepository (ExceptT UserRepositoryError IO)
  -> PasswordManager n
  -> Authenticator (ExceptT Error IO)
authenticator repo pm =
  Authenticator
    { authUser = authenticateUser repo pm
    }

-- |
-- Concrete implementation of 'AuthenticateUser'.
-- Depends on a 'UserRepository' and a 'PasswordManager'
authenticateUser
  :: UserRepository (ExceptT UserRepositoryError IO)
  -> PasswordManager n
  -> Credentials
  -> ExceptT Error IO (Id User)
authenticateUser userRepository passwordManager Credentials{username, password} = do
  (userId, user) <-
    withExceptT QueryError $ UserRepo.findByName userRepository username
  -- check whether the provided password is the correct one
  if validatePassword passwordManager user password
    then pure userId
    else throwE PasswordVerificationFailed
