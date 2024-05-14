module App
  ( run
  ) where

import           API.Application                      (app)
import           API.AppServices                      (start)
import qualified API.Config                           as Config
import qualified API.Types.Config                     as Config

import           CLIOptions                           (CLIOptions (configPath))
import qualified CLIOptions

import qualified Infrastructure.Database              as DB
import qualified Infrastructure.Logger                as Logger
import qualified Infrastructure.SystemTime            as SystemTime
import qualified Infrastructure.Types.Database        as DB
import qualified Infrastructure.Types.Logger          as Logger
import qualified Infrastructure.Types.SystemTime      as SystemTime

import qualified MatchOrNot.JSONWebKey                as JWK

import           Network.Wai                          (Application, Middleware)
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.Cors          (cors, corsRequestHeaders,
                                                       simpleCorsResourcePolicy)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

-- |
-- Main entry point of the application.
run :: IO ()
run = do
  options <- CLIOptions.parse
  appConfig <- Config.load $ configPath options
  key <- JWK.setup options

  withDeps appConfig $ \Deps{dbHandle, loggerHandle} -> do
    let (Config.Port port) = appConfig.api.apiPort
        services = start dbHandle loggerHandle key
        application = applyMiddleware (app services)

    Logger.logInfo loggerHandle $ "Starting app on port " <> show port <> "."
    Warp.run port application

-- Middleware

-- |
-- Apply CORS and a request logger middleware to the application.
applyMiddleware :: Application -> Application
applyMiddleware = corsMiddleware . logStdoutDev

corsMiddleware :: Middleware
corsMiddleware = cors (const . Just $ simpleCorsResourcePolicy{corsRequestHeaders = headers})
  where
    headers = ["Authorization", "Content-Type"]

-- Application dependencies

-- |
-- Aggregates all effects needed by the app
data Deps = Deps
  { systemTimeHandler :: SystemTime.Handle
  , loggerHandle      :: Logger.Handle
  , dbHandle          :: DB.Handle
  }

-- |
-- Starts dependencies and calls a given effectful function with them
withDeps :: Config.Config -> (Deps -> IO a) -> IO a
withDeps appConfig f = SystemTime.withHandle $ \systemTimeHandler ->
  Logger.withHandle systemTimeHandler $ \loggerHandle -> DB.withHandle appConfig $ \dbHandle -> f Deps{..}
