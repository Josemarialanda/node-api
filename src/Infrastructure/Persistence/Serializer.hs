module Infrastructure.Persistence.Serializer where

import Infrastructure.Persistence.Schema
  ( contentContent
  , contentId
  , contentUserId
  , tagId
  , tagName
  , userId
  , userName
  , userPassword
  )
import Infrastructure.Persistence.Schema qualified as DB
  ( Content (Content)
  , Tag (Tag)
  , User (User)
  )
import MatchOrNot.Content (Content (..), createContent)
import MatchOrNot.Id (Id)
import MatchOrNot.Owned (Owned (Owned))
import MatchOrNot.Owned qualified as Owned (content, userId)
import MatchOrNot.Tag (Tag (Tag))
import MatchOrNot.Tag qualified as Tag (name)
import MatchOrNot.User (User (User))
import MatchOrNot.User qualified as User (name, password)
import Rel8 (Result)

-- CONTENT

-- |
-- Transform from a domain representation of a 'Content' to its underlying database representation
serializeContent
  :: Id (Content Tag)
  -> Id User
  -> Content (Id Tag, Tag)
  -> (DB.Content Result, [DB.Tag Result])
serializeContent contentId' userId' content = (dbContent, dbTags)
  where
    dbContent =
      DB.Content
        { contentId = contentId'
        , contentContent = message content
        , contentUserId = userId'
        }
    dbTags = uncurry serializeTag <$> tags content

-- |
-- Transform from the database representation of a 'Content' to its domain representation
unserializeContent
  :: DB.Content Result -> [DB.Tag Result] -> DB.User Result -> Owned (Content Tag)
unserializeContent content tags' user =
  Owned
    { Owned.content =
        createContent
          (contentContent content)
          (unserializeTag <$> tags')
    , Owned.userId = userId user
    }

-- TAG

-- |
-- Transform from a domain representation of a 'Tag' to its underlying database representation
serializeTag :: Id Tag -> Tag -> DB.Tag Result
serializeTag uuid tag =
  DB.Tag
    { tagId = uuid
    , tagName = Tag.name tag
    }

-- |
-- Transform from the database representation of a 'Tag' to its domain representation
unserializeTag :: DB.Tag Result -> Tag
unserializeTag tag = Tag (tagName tag)

-- USER

-- |
-- Transform from a domain representation of a 'User' to its underlying database representation
serializeUser :: Id User -> User -> DB.User Result
serializeUser uuid user =
  DB.User
    { userId = uuid
    , userName = User.name user
    , userPassword = User.password user
    }

-- |
-- Transform from the database representation of a 'User' to its domain representation
unserializeUser :: DB.User Result -> User
unserializeUser user = User (userName user) (userPassword user)
