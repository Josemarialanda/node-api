module MatchOrNot.Types.EncryptedPassword (EncryptedPassword (..)) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson.Types (Parser, Value)
import Data.ByteString (ByteString)
import Data.Data (Proxy (Proxy))
import Data.OpenApi (Definitions, NamedSchema, Schema, ToSchema (declareNamedSchema))
import Data.OpenApi.Declare (Declare)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Rel8 (DBEq, DBType)

-- |
-- An 'EncryptedPassword' is a newtype wrapping a 'Bytestring'.
-- We do not export the constructor to enforce that an 'EncryptedPassword' is built using 'encryptPassword'
newtype EncryptedPassword = EncryptedPassword {asBytestring :: ByteString}
  deriving stock (Eq, Show, Read, Generic)
  deriving newtype (DBEq, DBType)

instance FromJSON EncryptedPassword where
  parseJSON :: Value -> Parser EncryptedPassword
  parseJSON json = EncryptedPassword . encodeUtf8 <$> parseJSON json

instance ToJSON EncryptedPassword where
  toJSON :: EncryptedPassword -> Value
  toJSON (EncryptedPassword s) = toJSON $ decodeUtf8 s

instance ToSchema EncryptedPassword where
  declareNamedSchema :: Proxy EncryptedPassword -> Declare (Definitions Schema) NamedSchema
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
