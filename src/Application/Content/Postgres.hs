module Application.Content.Postgres
  ( repository
  ) where

import           Control.Monad                      (forM)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Except         (ExceptT (ExceptT))

import           Core.Content                       (hasAllTags)
import           Core.Types.Content                 (Content, ContentRepository (..))
import           Core.Types.Id                      (Id (Id))
import           Core.Types.Owned                   (Owned (value))
import           Core.Types.Tag                     (Tag)
import           Core.Types.User                    (User)

import           Data.Tuple.Extra                   (uncurry3)
import           Data.UUID.V4                       (nextRandom)

import           Hasql.Session                      (QueryError)

import qualified Infrastructure.Database.Database   as DB
import qualified Infrastructure.Database.Queries    as DB (addContentWithTags, selectUserContents)
import           Infrastructure.Database.Serializer (serializeContent, unserializeContent)
import qualified Infrastructure.Types.Database      as DB

-- |
-- A 'ContentRepository' based on PostgreSQL
repository :: DB.Handle -> ContentRepository (ExceptT QueryError IO)
repository handle =
  ContentRepository
    { selectUserContentsByTags = postgresSelectUserContentsByTags handle
    , addContentWithTags = postgresAddContentWithTags handle
    }

postgresSelectUserContentsByTags :: DB.Handle -> Id User -> [Tag] -> ExceptT QueryError IO [Owned (Content Tag)]
postgresSelectUserContentsByTags handle userId tags = do
  -- Retrieve the user's contents data from the database
  userDBContents <- ExceptT $ DB.runQuery handle (DB.selectUserContents userId)
  -- Convert the contents data into their domain representation
  let userContents = uncurry3 unserializeContent <$> userDBContents
  -- Filter only the contents indexed by the provided tags
  pure $ filter (hasAllTags tags . value) userContents

postgresAddContentWithTags :: DB.Handle -> Id User -> Content Tag -> ExceptT QueryError IO (Id (Content Tag))
postgresAddContentWithTags handle userId content' = do
  -- Generate a UUID for the content
  contentUUID <- liftIO nextRandom
  -- Generate and associate a UUID to every tag
  contentWithTagsUUIDs <-
    liftIO $ forM content' (\tag -> (,tag) . Id <$> nextRandom)
  -- Run a transaction to add the content and its tags to the database
  ExceptT $
    DB.runQuery
      handle
      ( uncurry DB.addContentWithTags $
          serializeContent (Id contentUUID) userId contentWithTagsUUIDs
      )
  pure $ Id contentUUID
