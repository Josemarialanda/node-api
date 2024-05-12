module MatchOrNot.Types.Tag (Tag (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToParamSchema, ToSchema)
import Data.Text (Text)
import Servant (FromHttpApiData, ToHttpApiData)

-- |
-- A 'Tag' is a newtype wrapper around some 'Text', used to index a 'MatchOrNot.Content.Content'
newtype Tag = Tag {name :: Text}
  deriving stock (Eq, Show)
  deriving newtype (FromHttpApiData, ToHttpApiData, ToParamSchema, ToSchema, FromJSON, ToJSON)
