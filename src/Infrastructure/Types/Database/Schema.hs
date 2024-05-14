module Infrastructure.Types.Database.Schema
  ( Content (..)
  , ContentsTags (..)
  , Profile (..)
  , Tag (..)
  , User (..)
  ) where

import qualified Core.Types.Content           as Core (Content)
import           Core.Types.EncryptedPassword (EncryptedPassword)
import           Core.Types.Id                (Id)
import qualified Core.Types.Tag               as Core (Tag)
import qualified Core.Types.User              as Core (User)

import           Data.Text                    (Text)

import           GHC.Generics                 (Generic)

import           Rel8                         (Column, Rel8able)

-- TAG

-- |
-- The database representation of a 'Tag'
data Tag f = Tag
  { tagId   :: Column f (Id Core.Tag)
  , tagName :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- CONTENT

-- |
-- The database representation of a 'Content'
data Content f = Content
  { contentId      :: Column f (Id (Core.Content Core.Tag))
  , contentContent :: Column f Text
  , contentUserId  :: Column f (Id Core.User)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- CONTENTS_TAGS

-- |
-- The database representation of a connection between a 'Content' and a 'Tag'
data ContentsTags f = ContentsTags
  { ctContentId :: Column f (Id (Core.Content Core.Tag))
  , ctTagId     :: Column f (Id Core.Tag)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- USERS

-- |
-- The database representation of a 'User'
data User f = User
  { userId       :: Column f (Id Core.User)
  , userName     :: Column f Text
  , userPassword :: Column f EncryptedPassword
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- PROFILE

-- |
-- The database representation of a 'Profile'
data Profile f = Profile
  { profileFirstName :: Column f Text
  , profileLastName  :: Column f Text
  , profileAge       :: Column f Double
  , profileSex       :: Column f Text
  , profileUserId    :: Column f (Id Core.User)
  , profileEmail     :: Column f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)
