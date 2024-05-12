module Infrastructure.Types.Logger (Config (..), Context, Handle (..)) where

import Colog.Core (Severity (..))
import Data.Text (Text)
import Infrastructure.SystemTime qualified as SystemTime
import Prelude hiding (log)

newtype Config = Config {logLevel :: Severity}

data Handle = Handle
  { systemTimeHandle :: SystemTime.Handle
  , localContext :: Maybe Context
  , minLevel :: Severity
  }

type Context = Text

newtype Unquoted = Unquoted String

instance Show Unquoted where
  show :: Unquoted -> String
  show (Unquoted str) = str
