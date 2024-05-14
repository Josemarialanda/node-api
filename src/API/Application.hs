module API.Application
  ( app
  ) where

import API.Authentication                                  (authenticationServer)
import API.Docs                                            (docsServer)
import API.HealthCheck                                     (healthCheckServer)
import API.MatchOrNot                                      (matchOrNotServer)
import API.Types.Application                               (API, ApplicationAPI (..))
import API.Types.AppServices                               (AppServices (..))
import API.Types.MatchOrNot                                (MatchOrNotAPI)

import Core.Types.Content                                  (ContentRepository)
import Core.Types.Id                                       (Id)
import Core.Types.User                                     (User, UserRepository)

import Data.Proxy                                          (Proxy (..))

import Infrastructure.Types.Authentication.PasswordManager (PasswordManager)

import Network.Wai                                         (Application)

import Servant                                             (Context (EmptyContext, (:.)), Handler,
                                                            err401, serveWithContext)
import Servant.Auth.Server                                 (AuthResult (Authenticated),
                                                            ThrowAll (throwAll),
                                                            defaultCookieSettings)
import Servant.Server.Generic                              (AsServer)

-- |
-- The main application server, which serves the API
app :: AppServices -> Application
app appServices =
  serveWithContext
    (Proxy :: Proxy API)
    (defaultCookieSettings :. jwtSettings appServices :. EmptyContext)
    (server appServices)

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
      { matchOrNot = authenticatedMatchOrNotServer passwordManager userRepository contentRepository
      , docs = docsServer
      , healthCheck = healthCheckServer
      , authentication = authenticationServer passwordManager authenticateUser userRepository
      }

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
