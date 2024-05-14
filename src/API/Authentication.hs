module API.Authentication
  ( authenticationServer
  ) where

import           API.Types.Authentication                            (AuthenticationAPI (..))

import           Core.Types.Authentication.Authenticator             (Authenticator (authUser))
import           Core.Types.Authentication.Credentials               (Credentials (username))
import           Core.Types.Id                                       (Id)
import           Core.Types.User                                     (User, UserRepository)
import qualified Core.Types.User                                     as UserRepository

import           Infrastructure.Types.Authentication.PasswordManager (PasswordManager (..))
import           Infrastructure.Types.Authentication.Token           (Token)

import           Servant                                             (Handler)
import           Servant.Server.Generic                              (AsServer)

authenticationServer
  :: PasswordManager Handler
  -> Authenticator Handler
  -> UserRepository Handler
  -> AuthenticationAPI AsServer
authenticationServer passwordManager authHandler userRepository =
  AuthenticationAPI
    { register = registerEndpoint passwordManager userRepository
    , login = loginEndpoint passwordManager authHandler
    }

registerEndpoint
  :: PasswordManager Handler
  -> UserRepository Handler
  -> Credentials
  -> Handler (Id User)
registerEndpoint passwordManager userRepository login' = do
  -- hash the password
  hashedPassword <- generatePassword passwordManager login'
  -- store the new user into the database
  UserRepository.add userRepository (username login') hashedPassword

loginEndpoint
  :: PasswordManager Handler -> Authenticator Handler -> Credentials -> Handler Token
loginEndpoint passwordManager authHandler login' = do
  -- try to authenticate the user
  user <- authUser authHandler login'
  -- if the user authenticated, generate an authentication token
  generateToken passwordManager user
