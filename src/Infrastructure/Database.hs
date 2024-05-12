module Infrastructure.Database
  ( module Infrastructure.Types.Database
  , withHandle
  , runQuery
  )
where

import API.Config qualified as AppConfig
import API.Types.Config qualified as AppConfig
import Control.Exception (bracket)
import Data.ByteString.Char8 (unpack)
import Data.Maybe (fromMaybe)
import Hasql.Connection (acquire, release)
import Hasql.Session (QueryError, Session, run)
import Infrastructure.Types.Database (Config (..), Handle (..))

new :: Config -> IO Handle
new config = do
  eConn <- acquire . connectionString $ config
  either
    (fail . unpack . fromMaybe "unable to connect to the database")
    (pure . Handle)
    eConn

parseConfig :: AppConfig.Config -> Config
parseConfig = Config . (AppConfig.connectionString . AppConfig.database)

close :: Handle -> IO ()
close = release . dbConnection

withHandle :: AppConfig.Config -> (Handle -> IO a) -> IO a
withHandle config f = do
  bracket
    (new . parseConfig $ config)
    close
    f

runQuery :: Handle -> Session a -> IO (Either QueryError a)
runQuery handle query = run query (dbConnection handle)
