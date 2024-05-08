module API.Docs where

import API.Authentication (AuthenticationAPI)
import API.MatchOrNot (MatchOrNotAPI)
import Control.Lens ((&), (.~), (?~))
import Data.OpenApi (OpenApi, description, info, title, version)
import Data.Proxy (Proxy (Proxy))
import Servant (Get, JSON, NamedRoutes, Server, (:>))
import Servant.OpenApi (toOpenApi)

-- |
-- A single endpoint to expose the OpenAPI documentation of the application
type DocsAPI = "docs" :> Get '[JSON] OpenApi

docsServer :: Server DocsAPI
docsServer =
  return $
    toOpenApi (Proxy :: Proxy (NamedRoutes MatchOrNotAPI))
      <> toOpenApi (Proxy :: Proxy (NamedRoutes AuthenticationAPI))
      & info . title .~ "MatchOrNot api"
      & info . version .~ "1.0.0"
      & info . description ?~ "API endpoints for the matchOrNot API"
