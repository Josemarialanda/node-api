module Infrastructure.Database.Schema
  ( litUser
  , contentSchema
  , contentsTagsSchema
  , litContent
  , litTag
  , profileSchema
  , tagSchema
  , userSchema
  , litProfile
  ) where

import Infrastructure.Types.Database.Schema (Content (..), ContentsTags (..), Profile (..),
                                             Tag (..), User (..))

import Rel8                                 (Expr, Name, Result, TableSchema (..), lit)

-- TAG

-- |
-- A description of the schema of the 'Tag' table
tagSchema :: TableSchema (Tag Name)
tagSchema =
  TableSchema
    { name = "tags"
    , schema = Nothing
    , columns =
        Tag
          { tagId = "id"
          , tagName = "name"
          }
    }

-- |
-- Allows to lift a 'Tag' with no context into the 'Expr' context
litTag :: Tag Result -> Tag Expr
litTag (Tag id' name') = Tag (lit id') (lit name')

-- CONTENT

-- |
-- A description of the schema of the 'Content' table
contentSchema :: TableSchema (Content Name)
contentSchema =
  TableSchema
    { name = "contents"
    , schema = Nothing
    , columns =
        Content
          { contentId = "id"
          , contentContent = "content"
          , contentUserId = "user_id"
          }
    }

-- |
-- Allows to lift a 'Content' with no context into the 'Expr' context
litContent :: Content Result -> Content Expr
litContent (Content id' content' userId') = Content (lit id') (lit content') (lit userId')

-- CONTENTS_TAGS

-- |
-- A description of the schema of the 'ContentsTags' table
contentsTagsSchema :: TableSchema (ContentsTags Name)
contentsTagsSchema =
  TableSchema
    { name = "contents_tags"
    , schema = Nothing
    , columns =
        ContentsTags
          { ctContentId = "content_id"
          , ctTagId = "tag_id"
          }
    }

-- USERS

-- |
-- A description of the schema of the 'User' table
userSchema :: TableSchema (User Name)
userSchema =
  TableSchema
    { name = "users"
    , schema = Nothing
    , columns =
        User
          { userId = "id"
          , userName = "username"
          , userPassword = "password"
          }
    }

-- |
-- Allows to lift a 'User' with no context into the 'Expr' context
litUser :: User Result -> User Expr
litUser (User id' name' password) = User (lit id') (lit name') (lit password)

-- PROFILE

profileSchema :: TableSchema (Profile Name)
profileSchema =
  TableSchema
    { name = "user_profiles"
    , schema = Nothing
    , columns =
        Profile
          { profileFirstName = "first_name"
          , profileLastName = "last_name"
          , profileAge = "age"
          , profileSex = "sex"
          , profileUserId = "user_id"
          , profileEmail = "email"
          }
    }

litProfile :: Profile Result -> Profile Expr
litProfile (Profile firstName lastName age sex userId' email) =
  Profile (lit firstName) (lit lastName) (lit age) (lit sex) (lit userId') (lit email)
