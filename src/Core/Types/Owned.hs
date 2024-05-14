module Core.Types.Owned
  ( Owned (..)
  ) where

import Core.Types.Id   (Id)
import Core.Types.User (User)

import Data.Aeson      (FromJSON, ToJSON)
import Data.OpenApi    (ToSchema)

import GHC.Generics    (Generic)

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
