module API.Application where

import API.AppServices (AppServices (..))
import API.Authentication (AuthenticationAPI, authenticationServer)
import API.Docs (DocsAPI, docsServer)
import API.Healthcheck (HealthcheckAPI, healthcheckServer)
import API.MatchOrNot (MatchOrNotAPI, matchOrNotServer)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Infrastructure.Authentication.PasswordManager (PasswordManager)
import MatchOrNot.Id (Id)
import MatchOrNot.Repository.Content (ContentRepository)
import MatchOrNot.Repository.User (UserRepository)
import MatchOrNot.User (User)
import Network.Wai (Application)
import Servant (Context (EmptyContext, (:.)), Handler, err401, serveWithContext)
import Servant.API (NamedRoutes, type (:>))
import Servant.API.Generic ((:-))
import Servant.Auth (Auth, JWT)
import Servant.Auth.Server
  ( AuthResult (Authenticated)
  , ThrowAll (throwAll)
  , defaultCookieSettings
  )
import Servant.Server.Generic (AsServer)

type API = NamedRoutes ApplicationAPI

-- |
-- Collects all the API groups exposed by the application
data ApplicationAPI mode = ApplicationAPI
  { matchOrNot :: mode :- Auth '[JWT] (Id User) :> NamedRoutes MatchOrNotAPI
  , docs :: mode :- DocsAPI
  , healthcheck :: mode :- HealthcheckAPI
  , authentication :: mode :- NamedRoutes AuthenticationAPI
  }
  deriving stock (Generic)

-- |
-- For the endpoints which actually require authentication, checks whether the request provides a valid authentication token.
-- Otherwise it returns a 401 response
authenticatedMatchOrNotServer
  :: PasswordManager Handler
  -> UserRepository Handler
  -> ContentRepository Handler
  -> AuthResult (Id User)
  -> MatchOrNotAPI AsServer
authenticatedMatchOrNotServer passwordManager userRepository contentRepository = \case
  (Authenticated userId) -> matchOrNotServer userId passwordManager userRepository contentRepository
  _ -> throwAll err401

-- |
-- Setup all the application server, providing the services needed by the various endpoints
server :: AppServices -> ApplicationAPI AsServer
server
  AppServices
    { passwordManager
    , contentRepository
    , userRepository
    , authenticateUser
    } =
    ApplicationAPI
      { matchOrNot =
          authenticatedMatchOrNotServer passwordManager userRepository contentRepository
      , docs = docsServer
      , healthcheck = healthcheckServer
      , authentication =
          authenticationServer passwordManager authenticateUser userRepository
      }

app :: AppServices -> Application
app appServices =
  serveWithContext
    (Proxy :: Proxy API)
    (defaultCookieSettings :. jwtSettings appServices :. EmptyContext)
    (server appServices)
