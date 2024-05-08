module MatchOrNot.Repository.Content where

import MatchOrNot.Content (Content)
import MatchOrNot.Id (Id)
import MatchOrNot.Owned (Owned)
import MatchOrNot.Tag (Tag)
import MatchOrNot.User (User)

-- |
-- A 'ContentRepository' represents a collection of 'Content's.
-- It is indexed by a context 'm' which wraps the results.
data ContentRepository m = ContentRepository
  { selectUserContentsByTags :: Id User -> [Tag] -> m [Owned (Content Tag)]
  -- ^ selects all the 'Content's 'Owned' by a 'User' with a given 'Id' and indexed by all the provided 'Tag's
  , addContentWithTags :: Id User -> Content Tag -> m (Id (Content Tag))
  -- ^ adds a 'Content' indexed by some 'Tag's for a 'User' identified by a given 'Id'
  }

-- |
-- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'ContentRepository' is operating
hoist :: (forall a. m a -> n a) -> ContentRepository m -> ContentRepository n
hoist f ContentRepository{selectUserContentsByTags, addContentWithTags} =
  ContentRepository
    ((f .) . selectUserContentsByTags)
    ((f .) . addContentWithTags)
