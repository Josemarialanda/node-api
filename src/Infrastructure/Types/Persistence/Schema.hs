module Infrastructure.Types.Persistence.Schema
  ( Content (..)
  , ContentsTags (..)
  , Profile (..)
  , Tag (..)
  , User (..)
  ) where

import           Data.Text                          (Text)

import           GHC.Generics                       (Generic)

import qualified MatchOrNot.Types.Content           as Domain (Content)
import           MatchOrNot.Types.EncryptedPassword (EncryptedPassword)
import           MatchOrNot.Types.Id                (Id)
import qualified MatchOrNot.Types.Tag               as Domain (Tag)
import qualified MatchOrNot.Types.User              as Domain (User)

import           Rel8                               (Column, Rel8able)

-- TAG

-- |
-- The database representation of a 'Tag'
data Tag f = Tag
  { tagId   :: Column f (Id Domain.Tag)
  , tagName :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- CONTENT

-- |
-- The database representation of a 'Content'
data Content f = Content
  { contentId      :: Column f (Id (Domain.Content Domain.Tag))
  , contentContent :: Column f Text
  , contentUserId  :: Column f (Id Domain.User)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- CONTENTS_TAGS

-- |
-- The database representation of a connection between a 'Content' and a 'Tag'
data ContentsTags f = ContentsTags
  { ctContentId :: Column f (Id (Domain.Content Domain.Tag))
  , ctTagId     :: Column f (Id Domain.Tag)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- USERS

-- |
-- The database representation of a 'User'
data User f = User
  { userId       :: Column f (Id Domain.User)
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
  , profileUserId    :: Column f (Id Domain.User)
  , profileEmail     :: Column f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)
