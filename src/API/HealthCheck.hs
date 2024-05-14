module API.HealthCheck
  ( healthCheckServer
  ) where

import API.Types.HealthCheck (HealthcheckAPI)
import Servant (NoContent (NoContent), Server)

healthCheckServer :: Server HealthcheckAPI
healthCheckServer = pure NoContent
