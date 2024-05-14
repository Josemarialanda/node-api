module API.Types.HealthCheck
  ( HealthcheckAPI
  ) where

import Servant     (NoContent)
import Servant.API (Get, JSON, type (:>))

-- |
-- A single endpoint to check the liveness of the application
type HealthcheckAPI = "healthCheck" :> Get '[JSON] NoContent
