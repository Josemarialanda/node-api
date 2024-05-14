module MatchOrNot.Types.User
  ( UserRepository (..)
  , User (..)
  ) where

import Data.Aeson                         (ToJSON (toJSON), object, (.=))
import Data.Aeson.Types                   (Value)
import Data.OpenApi                       (ToSchema)
import Data.Text                          (Text)

import GHC.Generics                       (Generic)

import MatchOrNot.Types.EncryptedPassword (EncryptedPassword)
import MatchOrNot.Types.Id                (Id)
import MatchOrNot.Types.Profile           (Profile)

import Servant                            (NoContent)

-- |
-- A 'UserRespository' represents a collection of 'User's.
-- It is indexed by a context 'm' which wraps the results.
data UserRepository m = UserRepository
  { findByName         :: Text -> m (Id User, User)
  -- ^ Searches the repository for 'User's with the provided name
  , findById           :: Id User -> m (Id User, User)
  -- ^ Searches the repository for a 'User' with the provided 'Id'
  , add                :: Text -> EncryptedPassword -> m (Id User)
  -- ^ Adds a user with the provided name and password
  , deleteUserById     :: Id User -> m NoContent
  -- ^ Deletes a user with the provided 'Id'
  , updatePasswordById :: Id User -> EncryptedPassword -> m NoContent
  -- ^ Updates the password of a user with the provided 'Id'
  , updateUsernameById :: Id User -> Text -> m NoContent
  -- ^ Updates the username of a user with the provided 'Id'
  , getProfileById     :: Id User -> m Profile
  -- ^ Retrieves the profile of a user with the provided 'Id'
  , createProfileById  :: Id User -> Profile -> m NoContent
  -- ^ Creates a profile for a user with the provided 'Id'
  , updateProfileById  :: Id User -> Profile -> m NoContent
  -- ^ Updates the profile of a user with the provided 'Id'
  }

-- |
-- A 'User' contains a 'Text' and an 'EncryptedPassword'
data User = User
  { name     :: Text
  , password :: EncryptedPassword
  }
  deriving stock (Eq, Show, Read, Generic)

-- |
-- We need to be careful to hide the password (even if it is encrypted) when we expose an 'User'
instance ToJSON User where
  toJSON :: User -> Value
  toJSON User{name} = object ["name" .= name]

instance ToSchema User
