module MatchOrNot.User
  ( hoist
  ) where

import MatchOrNot.Types.User (UserRepository (..))

-- |
-- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'UserRepository' is operating
hoist :: (forall a. m a -> n a) -> UserRepository m -> UserRepository n
hoist f UserRepository{..} =
  UserRepository
    { findByName = f . findByName
    , findById = f . findById
    , add = (f .) . add
    , deleteUserById = f . deleteUserById
    , updatePasswordById = (f .) . updatePasswordById
    , updateUsernameById = (f .) . updateUsernameById
    , getProfileById = f . getProfileById
    , createProfileById = (f .) . createProfileById
    , updateProfileById = (f .) . updateProfileById
    }
