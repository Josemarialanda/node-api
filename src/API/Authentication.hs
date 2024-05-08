module API.Authentication where

import GHC.Generics (Generic)
import Infrastructure.Authentication.PasswordManager
  ( PasswordManager (generatePassword, generateToken)
  )
import Infrastructure.Authentication.Token (Token)
import MatchOrNot.Authentication.Authenticator (Authenticator)
import MatchOrNot.Authentication.Authenticator qualified as Authenticator
import MatchOrNot.Authentication.Credentials (Credentials (username))
import MatchOrNot.Id (Id)
import MatchOrNot.Repository.User as UserRepository
import MatchOrNot.User (User)
import Servant (Handler, JSON, Post, ReqBody, type (:>))
import Servant.API.Generic (type (:-))
import Servant.Server.Generic (AsServer)

-- |
-- The endpoints required to perform authentication
data AuthenticationAPI mode = AuthenticationAPI
  { register
      :: mode :- "register" :> ReqBody '[JSON] Credentials :> Post '[JSON] (Id User)
  -- ^ Given some 'Login' data, registers a new 'User'
  , login :: mode :- "login" :> ReqBody '[JSON] Credentials :> Post '[JSON] Token
  -- ^ Given some 'Login' data, generates an authentication token
  }
  deriving stock (Generic)

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
