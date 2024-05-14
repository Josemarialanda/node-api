module API.Types.AppServices
  ( AppServices (..)
  ) where

import Infrastructure.Types.Authentication.PasswordManager (PasswordManager)
import MatchOrNot.Authentication.Authenticator (Authenticator)
import MatchOrNot.Content (ContentRepository)
import MatchOrNot.Types.User (UserRepository)
import Prelude hiding (log)
import Servant (Handler)
import Servant.Auth.Server (JWTSettings)

-- |
-- Collection of services needed by the application to work
data AppServices = AppServices
  { jwtSettings       :: JWTSettings
  , passwordManager   :: PasswordManager Handler
  , contentRepository :: ContentRepository Handler
  , userRepository    :: UserRepository Handler
  , authenticateUser  :: Authenticator Handler
  }
