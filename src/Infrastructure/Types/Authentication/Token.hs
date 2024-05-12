module Infrastructure.Types.Authentication.Token where

import Data.Aeson
  ( FromJSON (parseJSON)
  , ToJSON (toJSON)
  , Value (String)
  , withText
  )

-- bytestring
import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import Data.Data (Proxy (Proxy))

-- openapi3
import Data.OpenApi (Definitions, NamedSchema, Schema, ToSchema (declareNamedSchema))

-- text

import Data.Aeson.Types (Parser)
import Data.OpenApi.Declare (Declare)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

-- |
-- An authentication 'Token'
newtype Token = Token ByteString
  deriving newtype (Show)

instance FromJSON Token where
  parseJSON :: Value -> Parser Token
  parseJSON = withText "Token" (pure . Token . fromStrict . encodeUtf8)

instance ToJSON Token where
  toJSON :: Token -> Value
  toJSON (Token bs) = String . decodeUtf8 $ toStrict bs

instance ToSchema Token where
  declareNamedSchema :: Proxy Token -> Declare (Definitions Schema) NamedSchema
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
