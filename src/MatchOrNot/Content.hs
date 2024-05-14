module MatchOrNot.Content
  ( createContent
  , hasAllTags
  , hoist
  ) where


import Data.List                (nub)
import Data.Text                (Text)

import MatchOrNot.Types.Content (Content (..), ContentRepository (..))

createContent :: Eq tag => Text -> [tag] -> Content tag
createContent message tags = Content message (nub tags)

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
