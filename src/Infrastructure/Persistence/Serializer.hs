module Infrastructure.Persistence.Serializer (serializeContent, unserializeContent, serializeUser, unserializeUser, serializeProfile, unserializeProfile) where

import Infrastructure.Types.Persistence.Schema
  ( contentContent
  , contentId
  , contentUserId
  , profileAge
  , profileEmail
  , profileFirstName
  , profileLastName
  , profileSex
  , tagId
  , tagName
  , userId
  , userName
  , userPassword
  )
import Infrastructure.Types.Persistence.Schema qualified as DB
  ( Content (Content)
  , Profile (..)
  , Tag (Tag)
  , User (User)
  )
import MatchOrNot.Types.Content (Content (..), createContent)
import MatchOrNot.Types.Id (Id)
import MatchOrNot.Types.Owned (Owned (Owned))
import MatchOrNot.Types.Owned qualified as Owned (userId, value)
import MatchOrNot.Types.Profile (Profile (..))
import MatchOrNot.Types.Tag (Tag (Tag))
import MatchOrNot.Types.Tag qualified as Tag (name)
import MatchOrNot.Types.User (User (User))
import MatchOrNot.Types.User qualified as User (name, password)
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
    { Owned.value = createContent (contentContent content) (unserializeTag <$> tags')
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

-- PROFILE

-- |
-- Transform from a domain representation of a 'Profile' to its underlying database representation
serializeProfile
  :: Id User
  -> Profile
  -> DB.Profile Result
serializeProfile userId' Profile{..} =
  DB.Profile
    { profileFirstName = firstName
    , profileLastName = lastName
    , profileAge = age
    , profileSex = sex
    , profileUserId = userId'
    , profileEmail = email
    }

-- |
-- Transform from the database representation of a 'Profile' to its domain representation
unserializeProfile :: DB.Profile Result -> Profile
unserializeProfile profile =
  Profile
    { firstName = profileFirstName profile
    , lastName = profileLastName profile
    , age = profileAge profile
    , sex = profileSex profile
    , email = profileEmail profile
    }
