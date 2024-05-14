module API.Types.Config
  ( Config (..)
  , ApiConfig (..)
  , DatabaseConfig (..)
  , Host (..)
  , Port (..)
  , DBName (..)
  , User (..)
  , Password (..)
  ) where

import Data.Text (Text)

-- |
-- The whole config needed by the application
data Config = Config
  { database :: DatabaseConfig
  , api      :: ApiConfig
  }

-- |
-- The configuration parameters needed to expose the API
newtype ApiConfig = ApiConfig {apiPort :: Port}

-- |
-- The configuration parameters needed to connect to a database
data DatabaseConfig = DatabaseConfig
  { host     :: Host
  , port     :: Port
  , dbname   :: DBName
  , user     :: User
  , password :: Password
  }

newtype Host = Host {getHost :: Text}

newtype Port = Port {getPort :: Int}
  deriving newtype (Show)

newtype DBName = DBName {getDBName :: Text}

newtype User = User {getUser :: Text}

newtype Password = Password {getPassword :: Text}
