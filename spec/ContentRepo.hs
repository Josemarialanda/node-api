module ContentRepo (Table, repository) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.Map.Lazy (Map, elems, filter, insert)
import Data.UUID.V4 (nextRandom)
import GHC.Conc (TVar, atomically, readTVar, writeTVar)
import Hasql.Session (QueryError)
import MatchOrNot.Content
  ( Content
  , ContentRepository (..)
  , hasAllTags
  )
import MatchOrNot.Types.Id (Id (Id))
import MatchOrNot.Types.Owned (Owned (..))
import MatchOrNot.Types.Tag (Tag)
import MatchOrNot.Types.User (User)
import Prelude hiding (filter)

type Table = TVar (Map (Id (Content Tag)) (Owned (Content Tag)))

repository :: Table -> ContentRepository (ExceptT QueryError IO)
repository contentsMap =
  ContentRepository
    { selectUserContentsByTags = inMemorySelectUserContentsByTags contentsMap
    , addContentWithTags = inMemoryAddContentWithTags contentsMap
    }

inMemorySelectUserContentsByTags
  :: TVar (Map (Id (Content Tag)) (Owned (Content Tag)))
  -> Id User
  -> [Tag]
  -> ExceptT QueryError IO [Owned (Content Tag)]
inMemorySelectUserContentsByTags contentsMap userId' tags = liftIO . atomically $ do
  contents <- readTVar contentsMap
  let userContentsWithTags =
        filter
          ((&&) <$> ((== userId') . userId) <*> (hasAllTags tags . value))
          contents
  pure $ elems userContentsWithTags

inMemoryAddContentWithTags
  :: TVar (Map (Id (Content Tag)) (Owned (Content Tag)))
  -> Id User
  -> Content Tag
  -> ExceptT QueryError IO (Id (Content Tag))
inMemoryAddContentWithTags contentsMap userId' content' = do
  contentId <- Id <$> liftIO nextRandom
  liftIO . atomically $ do
    contents <- readTVar contentsMap
    writeTVar contentsMap $ insert contentId (Owned userId' content') contents
  pure contentId
