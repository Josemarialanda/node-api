module API.HealthCheck (module API.Types.HealthCheck, healthCheckServer) where

import API.Types.HealthCheck
import Servant (NoContent (NoContent), Server)

healthCheckServer :: Server HealthcheckAPI
healthCheckServer = pure NoContent
