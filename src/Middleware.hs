module Middleware (apply) where

import Network.Wai (Application, Middleware)
import Network.Wai.Middleware.Cors
  ( cors
  , corsRequestHeaders
  , simpleCorsResourcePolicy
  )
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

apply :: Application -> Application
apply =
  corsMiddleware . logStdoutDev

corsMiddleware :: Middleware
corsMiddleware =
  let headers = ["Authorization", "Content-Type"]
  in cors (const . Just $ simpleCorsResourcePolicy{corsRequestHeaders = headers})
