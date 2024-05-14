module Impl.Authentication
  ( authenticator
  ) where

import           Control.Monad.Trans.Except                          (ExceptT, throwE, withExceptT)

import           Impl.Types.Authentication                           (Error (..))
import           Impl.Types.User.Error                               (UserRepositoryError)

import           Infrastructure.Types.Authentication.PasswordManager (PasswordManager (validatePassword))

import           MatchOrNot.Authentication.Authenticator             (Authenticator (..))
import           MatchOrNot.Authentication.Credentials               (Credentials (..))
import           MatchOrNot.Types.Id                                 (Id)
import           MatchOrNot.Types.User                               (User, UserRepository)
import qualified MatchOrNot.Types.User                               as UserRepo (findByName)

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
