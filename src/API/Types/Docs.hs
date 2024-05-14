module API.Types.Docs
  ( DocsAPI
  ) where

import Data.OpenApi (OpenApi)

import Servant      (Get, JSON, (:>))

-- |
-- A single endpoint to expose the OpenAPI documentation of the application
type DocsAPI = "docs" :> Get '[JSON] OpenApi
