module MatchOrNot.Types.Profile (Profile (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

data Sex = Male | Female | Other Text
  deriving stock (Eq, Show, Read, Generic)

instance ToJSON Sex
instance FromJSON Sex
instance ToSchema Sex

-- |
-- A 'Profile' represents a user's public information.
data Profile = Profile
  { firstName :: Text
  , lastName :: Text
  , age :: Double
  , sex :: Sex
  }
  deriving stock (Eq, Show, Read, Generic)

instance ToJSON Profile
instance FromJSON Profile
instance ToSchema Profile
