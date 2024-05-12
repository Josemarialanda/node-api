module API.Types.AppServices (AppServices (..)) where

import Infrastructure.Authentication.PasswordManager (PasswordManager)
import MatchOrNot.Authentication.Authenticator qualified as Auth
import MatchOrNot.Content (ContentRepository)
import MatchOrNot.Types.User (UserRepository)
import Servant (Handler)
import Servant.Auth.Server (JWTSettings)
import Prelude hiding (log)

-- |
-- Collection of services needed by the application to work
data AppServices = AppServices
  { jwtSettings :: JWTSettings
  , passwordManager :: PasswordManager Handler
  , contentRepository :: ContentRepository Handler
  , userRepository :: UserRepository Handler
  , authenticateUser :: Auth.Authenticator Handler
  }
