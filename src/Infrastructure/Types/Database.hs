module Infrastructure.Types.Database (Config (..), Handle (..)) where

import Data.ByteString.Char8 (ByteString)
import Hasql.Connection (Connection)

newtype Config = Config {connectionString :: ByteString}

newtype Handle = Handle {dbConnection :: Connection}
