module API.Authentication (authenticationServer) where

import API.Types.Authentication (AuthenticationAPI (..))
import Infrastructure.Types.Authentication.PasswordManager
  ( PasswordManager (generatePassword, generateToken)
  )
import Infrastructure.Types.Authentication.Token (Token)
import MatchOrNot.Authentication.Authenticator (Authenticator)
import MatchOrNot.Authentication.Authenticator qualified as Authenticator
import MatchOrNot.Authentication.Credentials (Credentials (username))
import MatchOrNot.Types.Id (Id)
import MatchOrNot.Types.User (User, UserRepository)
import MatchOrNot.Types.User qualified as UserRepository
import Servant (Handler)
import Servant.Server.Generic (AsServer)

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
  user <- Authenticator.authUser authHandler login'
  -- if the user authenticated, generate an authentication token
  generateToken passwordManager user
