module API.Docs
  ( docsServer
  ) where

import API.Types.Authentication (AuthenticationAPI)
import API.Types.Docs           (DocsAPI)
import API.Types.MatchOrNot     (MatchOrNotAPI)

import Control.Lens             ((&), (.~), (?~))

import Data.OpenApi             (description, info, title, version)
import Data.Proxy               (Proxy (Proxy))

import Servant                  (NamedRoutes, Server)
import Servant.OpenApi          (toOpenApi)

docsServer :: Server DocsAPI
docsServer =
  return $
    toOpenApi (Proxy :: Proxy (NamedRoutes MatchOrNotAPI))
      <> toOpenApi (Proxy :: Proxy (NamedRoutes AuthenticationAPI))
      & info . title .~ "Core api"
      & info . version .~ "1.0.0"
      & info . description ?~ "API endpoints for the matchOrNot API"
