module Infrastructure.Database.Serializer
  ( serializeContent
  , unserializeContent
  , serializeUser
  , unserializeUser
  , serializeProfile
  , unserializeProfile
  ) where

import           Core.Content                         (createContent)
import           Core.Types.Content                   (Content (..))
import           Core.Types.Id                        (Id)
import           Core.Types.Owned                     (Owned (Owned))
import qualified Core.Types.Owned                     as Owned (userId, value)
import           Core.Types.Profile                   (Profile (..))
import           Core.Types.Tag                       (Tag (Tag))
import qualified Core.Types.Tag                       as Tag (name)
import           Core.Types.User                      (User (User))
import qualified Core.Types.User                      as User (name, password)

import           Infrastructure.Types.Database.Schema (contentContent, contentId, contentUserId,
                                                       profileAge, profileEmail, profileFirstName,
                                                       profileLastName, profileSex, tagId, tagName,
                                                       userId, userName, userPassword)
import qualified Infrastructure.Types.Database.Schema as DB (Content (Content), Profile (..),
                                                             Tag (Tag), User (User))

import           Rel8                                 (Result)

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
