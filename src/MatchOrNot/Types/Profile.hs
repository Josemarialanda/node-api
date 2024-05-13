module MatchOrNot.Types.Profile (Profile (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

-- |
-- A 'Profile' represents a user's public information.
data Profile = Profile
  { firstName :: Text
  , lastName :: Text
  , age :: Double
  , sex :: Text
  , email :: Maybe Text
  }
  deriving stock (Eq, Show, Read, Generic)

instance ToJSON Profile
instance FromJSON Profile
instance ToSchema Profile
