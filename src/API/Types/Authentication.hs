module API.Types.Authentication (AuthenticationAPI (..)) where

import GHC.Generics (Generic)
import Infrastructure.Types.Authentication.Token (Token)
import MatchOrNot.Authentication.Credentials (Credentials)
import MatchOrNot.Types.Id (Id)
import MatchOrNot.Types.User (User)
import Servant (JSON, Post, ReqBody, type (:>))
import Servant.API.Generic (type (:-))

-- |
-- The endpoints required to perform authentication
data AuthenticationAPI mode = AuthenticationAPI
  { register
      :: mode :- "register" :> ReqBody '[JSON] Credentials :> Post '[JSON] (Id User)
  -- ^ Given some 'Login' data, registers a new 'User'
  , login :: mode :- "login" :> ReqBody '[JSON] Credentials :> Post '[JSON] Token
  -- ^ Given some 'Login' data, generates an authentication token
  }
  deriving stock (Generic)
