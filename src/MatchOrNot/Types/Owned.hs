module MatchOrNot.Types.Owned
  ( Owned (..)
  ) where

import Data.Aeson            (FromJSON, ToJSON)
import Data.OpenApi          (ToSchema)

import GHC.Generics          (Generic)

import MatchOrNot.Types.Id   (Id)
import MatchOrNot.Types.User (User)

-- |
-- 'Owned' is a data type used to associate a 'User' to a value of type 'a'.
data Owned a = Owned
  { userId :: Id User
  , value  :: a
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON a => FromJSON (Owned a)

instance ToJSON a => ToJSON (Owned a)

instance ToSchema a => ToSchema (Owned a)
