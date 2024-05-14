module API.Types.Application
  ( API
  , ApplicationAPI (..)
  ) where

import API.Types.Authentication (AuthenticationAPI)
import API.Types.Docs           (DocsAPI)
import API.Types.HealthCheck    (HealthcheckAPI)
import API.Types.MatchOrNot     (MatchOrNotAPI)

import GHC.Generics             (Generic)

import MatchOrNot.Types.Id      (Id)
import MatchOrNot.Types.User    (User)

import Servant.API              (NamedRoutes, type (:>))
import Servant.API.Generic      ((:-))
import Servant.Auth             (Auth, JWT)

type API = NamedRoutes ApplicationAPI

-- |
-- Collects all the API groups exposed by the application
data ApplicationAPI mode = ApplicationAPI
  { matchOrNot     :: mode :- Auth '[JWT] (Id User) :> NamedRoutes MatchOrNotAPI
  , docs           :: mode :- DocsAPI
  , healthCheck    :: mode :- HealthcheckAPI
  , authentication :: mode :- NamedRoutes AuthenticationAPI
  }
  deriving stock (Generic)
