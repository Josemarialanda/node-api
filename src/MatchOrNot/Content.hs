module MatchOrNot.Content (Content (message, tags), createContent, hasAllTags, ContentRepository (..), hoist) where

import MatchOrNot.Types.Content (Content (message, tags), createContent)
import MatchOrNot.Types.Id (Id)
import MatchOrNot.Types.Owned (Owned)
import MatchOrNot.Types.Tag (Tag)
import MatchOrNot.Types.User (User)

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

-- |
-- checks whether a 'Content' is indexed by all the provided 'tag's
hasAllTags :: Eq tag => [tag] -> Content tag -> Bool
hasAllTags tags' content = all (\tag -> tag `elem` tags content) tags'
