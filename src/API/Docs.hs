module API.Docs
  ( docsServer
  ) where

import API.Types.Authentication (AuthenticationAPI)
import API.Types.Docs           (DocsAPI)
import API.Types.Main           (MainAPI)

import Control.Lens             ((&), (.~), (?~))

import Data.OpenApi             (description, info, title, version)
import Data.Proxy               (Proxy (Proxy))

import Servant                  (NamedRoutes, Server)
import Servant.OpenApi          (toOpenApi)

docsServer :: Server DocsAPI
docsServer =
  return $
    toOpenApi (Proxy :: Proxy (NamedRoutes MainAPI))
      <> toOpenApi (Proxy :: Proxy (NamedRoutes AuthenticationAPI))
      & info . title .~ "Core api"
      & info . version .~ "1.0.0"
      & info . description ?~ "API endpoints for the main authenticated API"
