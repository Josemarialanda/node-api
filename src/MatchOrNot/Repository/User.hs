module MatchOrNot.Repository.User (UserRepository (..), hoist) where

import Data.Text (Text)
import MatchOrNot.EncryptedPassword (EncryptedPassword)
import MatchOrNot.Id (Id)
import MatchOrNot.User (User)
import Servant (NoContent)

-- |
-- A 'UserRespository' represents a collection of 'User's.
-- It is indexed by a context 'm' which wraps the results.
data UserRepository m = UserRepository
  { findByName :: Text -> m (Id User, User)
  -- ^ Searches the repository for 'User's with the provided name
  , findById :: Id User -> m (Id User, User)
  -- ^ Searches the repository for a 'User' with the provided 'Id'
  , add :: Text -> EncryptedPassword -> m (Id User)
  -- ^ Adds a user with the provided name and password
  , deleteUserById :: Id User -> m NoContent
  -- ^ Deletes a user with the provided 'Id'
  , changePasswordById :: Id User -> EncryptedPassword -> m NoContent
  -- ^ Changes the password of a user with the provided 'Id'
  , changeUsernameById :: Id User -> Text -> m NoContent
  -- ^ Changes the username of a user with the provided 'Id'
  }

-- |
-- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'UserRepository' is operating
hoist :: (forall a. m a -> n a) -> UserRepository m -> UserRepository n
hoist f UserRepository{..} =
  UserRepository
    (f . findByName)
    (f . findById)
    ((f .) . add)
    (f . deleteUserById)
    ((f .) . changePasswordById)
    ((f .) . changeUsernameById)
