module API.Types.AppServices
  ( AppServices (..)
  ) where

import Core.Types.Authentication.Authenticator             (Authenticator)
import Core.Types.Content                                  (ContentRepository)
import Core.Types.User                                     (UserRepository)

import Infrastructure.Types.Authentication.PasswordManager (PasswordManager)

import Prelude                                             hiding (log)

import Servant                                             (Handler)
import Servant.Auth.Server                                 (JWTSettings)

-- |
-- Collection of services needed by the application to work
data AppServices = AppServices
  { jwtSettings       :: JWTSettings
  , passwordManager   :: PasswordManager Handler
  , contentRepository :: ContentRepository Handler
  , userRepository    :: UserRepository Handler
  , authenticateUser  :: Authenticator Handler
  }
