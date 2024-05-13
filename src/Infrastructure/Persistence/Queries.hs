module Infrastructure.Persistence.Queries
  ( module Infrastructure.Types.Persistence.Queries
  , addContentWithTags
  , addUser
  , updatePasswordById
  , updateUsernameById
  , deleteUserById
  , getUserProfile
  , updateUserProfile
  , selectUserById
  , selectUserByName
  , selectUserContents
  , createUserProfile
  ) where

import Data.List qualified as List (filter)
import Data.Text (Text)
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement)
import Hasql.Transaction qualified as Transaction (statement)
import Hasql.Transaction.Sessions
  ( IsolationLevel (Serializable)
  , Mode (Write)
  , transaction
  )
import Infrastructure.Persistence.Schema
  ( Content (..)
  , ContentsTags (..)
  , Profile
  , Tag (..)
  , User (..)
  , contentSchema
  , contentsTagsSchema
  , litContent
  , litTag
  , profileAge
  , profileEmail
  , profileFirstName
  , profileLastName
  , profileSchema
  , profileSex
  , profileUserId
  , tagSchema
  , userId
  , userSchema
  )
import Infrastructure.Types.Persistence.Queries
  ( WrongNumberOfResults (..)
  )
import MatchOrNot.EncryptedPassword (EncryptedPassword)
import MatchOrNot.Types.Id (Id)
import MatchOrNot.Types.Profile qualified as Profile
import MatchOrNot.Types.User qualified as Domain (User)
import Rel8
  ( Delete (..)
  , Expr
  , Insert (..)
  , Name
  , OnConflict (..)
  , Query
  , Rel8able
  , Result
  , TableSchema
  , Update (..)
  , delete
  , each
  , filter
  , in_
  , insert
  , lit
  , many
  , select
  , update
  , values
  , where_
  , (==.)
  )
import Prelude hiding (filter)

-- SELECT CONTENTS

-- |
-- Selects the 'ContentsTags' for a given 'Content'
contentsTagsForContent :: Content Expr -> Query (ContentsTags Expr)
contentsTagsForContent content =
  each contentsTagsSchema
    >>= filter
      ( \contentTag' ->
          ctContentId contentTag' ==. contentId content
      )

-- |
-- Selects the 'Tags' associated with a given 'Content'
tagsForContent :: Content Expr -> Query (Tag Expr)
tagsForContent content = do
  tag <- each tagSchema
  contentTag' <- contentsTagsForContent content
  where_ $ tagId tag ==. ctTagId contentTag'
  return tag

-- |
-- Selects the 'User' who ownes a 'Content'
userForContent :: Content Expr -> Query (User Expr)
userForContent content =
  each userSchema
    >>= filter
      ( \user ->
          userId user ==. contentUserId content
      )

-- |
-- Given a 'Domain.User' 'Id', retrieves all the contents for that specific user
selectUserContents
  :: Id Domain.User -> Session [(Content Result, [Tag Result], User Result)]
selectUserContents userId' = statement () . select $ do
  -- Select all content for the given user
  content <-
    each contentSchema
      >>= filter
        ( \content ->
            contentUserId content ==. lit userId'
        )
  -- Select tags for each content
  tags <- many $ tagsForContent content
  -- Select user for each content
  user <- userForContent content
  return (content, tags, user)

-- SELECT TAGS

-- |
-- Selects all tags present in the database among the requested ones
selectTags :: [Tag Result] -> Statement () [Tag Result]
selectTags tagNames =
  select $
    each tagSchema >>= filter ((`in_` (tagName . litTag <$> tagNames)) . tagName)

-- ADD CONTENT

-- |
-- Adds a number of rows to the specified 'TableSchema'
add :: Rel8able f => TableSchema (f Name) -> [f Expr] -> Statement () ()
add schema rows' =
  insert $
    Insert
      { into = schema
      , rows = values rows'
      , onConflict = Abort
      , returning = pure ()
      }

-- |
-- Creates a 'ContentTag' given a 'Content' and a 'Tag'
contentTag :: Content f -> Tag f -> ContentsTags f
contentTag content tag =
  ContentsTags
    { ctContentId = contentId content
    , ctTagId = tagId tag
    }

-- |
-- Removes the 'alreadyPresentTags' from 'allTags'
removeAlreadyPresentTags :: [Tag Result] -> [Tag Result] -> [Tag Result]
removeAlreadyPresentTags allTags alreadyPresentTags =
  List.filter
    (\tag -> tagName tag `notElem` (tagName <$> alreadyPresentTags))
    allTags

-- |
-- Given a 'Content' and a list of 'Tag's, it inserts the new content into the database associating to it the provided tags.
-- To avoid 'Tag' repetitions, it goes through the following steps:
--
-- * selects 'Tag's from the database
-- * replaces the generated 'UUID's with the one coming from the database
-- * inserts the new 'Tag's
-- * inserts the 'Content'
-- * inserts the 'ContentsTags' to link the 'Content' with its 'Tags'
addContentWithTags :: Content Result -> [Tag Result] -> Session ()
addContentWithTags content tags = transaction Serializable Write $ do
  alreadyPresentTags <- Transaction.statement () (selectTags tags)
  let newTags = litTag <$> removeAlreadyPresentTags tags alreadyPresentTags
  Transaction.statement () $ add tagSchema newTags
  Transaction.statement () $ add contentSchema [litContent content]
  Transaction.statement () $
    add
      contentsTagsSchema
      (contentTag (litContent content) <$> (litTag <$> alreadyPresentTags) <> newTags)

-- SELECT USER BY USERNAME

-- |
-- Given a list of results, succeed if there is only one in the list, otherwise fail with the appropriate error message
justOne :: [a Result] -> Either WrongNumberOfResults (a Result)
justOne = \case
  [] -> Left NoResults
  [a] -> Right a
  _ -> Left MoreThanOneResult

-- |
-- Retrieve from the database a user with the provided name.
-- If in the database we find none or more the one, it returns the appropriate error message
selectUserByName :: Text -> Session (Either WrongNumberOfResults (User Result))
selectUserByName name = statement () query
  where
    query = fmap justOne . select $ do
      users <- each userSchema
      filter (\user -> userName user ==. lit name) users

-- |
-- Retrieve from the database a user with the provided 'Id'.
-- If in the database we find none or more the one, it returns the appropriate error message
selectUserById :: Id Domain.User -> Session (Either WrongNumberOfResults (User Result))
selectUserById userId' = statement () query
  where
    query = fmap justOne . select $ do
      users <- each userSchema
      filter (\user -> userId user ==. lit userId') users

-- |
-- Update the password of the 'User' with the provided 'Id'
-- If the 'User' does not exist, it returns the appropriate error message
updatePasswordById :: Id Domain.User -> EncryptedPassword -> Session ()
updatePasswordById userId' newPassword = statement () query
  where
    query =
      update $
        Update
          { target = userSchema
          , from = pure ()
          , updateWhere = \_ row -> userId row ==. lit userId'
          , set = \_ row -> row{userPassword = lit newPassword}
          , returning = pure ()
          }

-- |
-- Update the username of the 'User' with the provided 'Id'
-- If the 'User' does not exist, it returns the appropriate error message
updateUsernameById :: Id Domain.User -> Text -> Session ()
updateUsernameById userId' newUsername = statement () query
  where
    query =
      update $
        Update
          { target = userSchema
          , from = pure ()
          , updateWhere = \_ row -> userId row ==. lit userId'
          , set = \_ row -> row{userName = lit newUsername}
          , returning = pure ()
          }

-- DELETE USER

-- |
-- Deletes a 'User' from the database given its 'Id'
deleteUserById :: Id Domain.User -> Session ()
deleteUserById userId' = statement () query
  where
    query =
      delete $
        Delete
          { from = userSchema
          , using = pure ()
          , deleteWhere = \_ usr -> userId usr ==. lit userId'
          , returning = pure ()
          }

-- ADD USER

-- |
-- Add a new 'User' in the database
addUser :: User Expr -> Session ()
addUser = statement () . add userSchema . pure

-- GET USER PROFILE

-- |
-- Given a 'User' 'Id', retrieves the 'User' and its 'Profile'
-- If in the database we find none or more the one user, it returns the appropriate error message
getUserProfile :: Id Domain.User -> Session (Either WrongNumberOfResults (Profile Result))
getUserProfile userId' = statement () query
  where
    query = fmap justOne . select $ do
      profiles <- each profileSchema
      filter (\profile -> profileUserId profile ==. lit userId') profiles

-- |
-- Update the 'Profile' of the 'User' with the provided 'Id'
-- If the 'User' does not exist, do nothing
updateUserProfile :: Id Domain.User -> Profile.Profile -> Session ()
updateUserProfile userId' profile = statement () query
  where
    query =
      update $
        Update
          { target = profileSchema
          , from = pure ()
          , updateWhere = \_ row -> profileUserId row ==. lit userId'
          , set = \_ row ->
              row
                { profileFirstName = lit $ Profile.firstName profile
                , profileLastName = lit $ Profile.lastName profile
                , profileAge = lit $ Profile.age profile
                , profileSex = lit $ Profile.sex profile
                , profileEmail = lit $ Profile.email profile
                }
          , returning = pure ()
          }

createUserProfile :: Profile Expr -> Session ()
createUserProfile = statement () . add profileSchema . pure
