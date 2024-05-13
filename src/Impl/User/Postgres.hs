module Impl.User.Postgres (module Impl.Types.User.Error, repository) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE, withExceptT)
import Data.ByteString (isInfixOf)
import Data.Text (Text)
import Data.UUID.V4 (nextRandom)
import Hasql.Session
  ( CommandError (ResultError)
  , QueryError (QueryError)
  , ResultError (ServerError)
  , Session
  )
import Impl.Types.User.Error (UserRepositoryError (..))
import Infrastructure.Database qualified as DB
import Infrastructure.Persistence.Queries qualified as Query
import Infrastructure.Persistence.Schema (litProfile, litUser, userId)
import Infrastructure.Persistence.Serializer (serializeProfile, serializeUser, unserializeProfile, unserializeUser)
import MatchOrNot.Types.EncryptedPassword (EncryptedPassword)
import MatchOrNot.Types.Id (Id (Id))
import MatchOrNot.Types.Profile (Profile)
import MatchOrNot.Types.User (User (User), UserRepository (..))
import Servant (NoContent (NoContent))

-- |
-- A 'UserRepository' based on PostgreSQL
repository :: DB.Handle -> UserRepository (ExceptT UserRepositoryError IO)
repository handle =
  UserRepository
    { findByName = postgresGetUserByName handle
    , findById = postgresGetUserById handle
    , add = postgresAddUser handle
    , deleteUserById = postgresDeleteUser handle
    , updatePasswordById = postgresUpdatePasswordById handle
    , updateUsernameById = postgresUpdateUsernameById handle
    , getProfileById = postgresGetProfileById handle
    , createProfileById = postgresCreateProfileById handle
    , updateProfileById = postgresUpdateProfileById handle
    }

postgresGetUserByName :: DB.Handle -> Text -> ExceptT UserRepositoryError IO (Id User, User)
postgresGetUserByName handle name = do
  eitherUser <- runRepositoryQuery handle (Query.selectUserByName name)
  case eitherUser of
    Right usr -> pure (userId usr, unserializeUser usr)
    Left e -> throwE $ UnexpectedNumberOfRows e

postgresGetUserById :: DB.Handle -> Id User -> ExceptT UserRepositoryError IO (Id User, User)
postgresGetUserById handle userId = do
  eitherUser <- runRepositoryQuery handle (Query.selectUserById userId)
  case eitherUser of
    Right usr -> pure (userId, unserializeUser usr)
    Left e -> throwE $ UnexpectedNumberOfRows e

postgresAddUser
  :: DB.Handle
  -> Text
  -> EncryptedPassword
  -> ExceptT UserRepositoryError IO (Id User)
postgresAddUser handle name password = do
  -- Generate the UUID for the user
  userId' <- liftIO nextRandom
  let query = Query.addUser . litUser $ serializeUser (Id userId') (User name password)
  -- Actually add the user to the database, differentiating the `UserRepositoryError` cases
  runRepositoryQuery handle query
  pure $ Id userId'

postgresDeleteUser :: DB.Handle -> Id User -> ExceptT UserRepositoryError IO NoContent
postgresDeleteUser handle userId = do
  eitherUser <- runRepositoryQuery handle (Query.selectUserById userId)
  case eitherUser of
    Right _ -> do
      runRepositoryQuery handle (Query.deleteUserById userId)
      pure NoContent
    Left e -> throwE $ UnexpectedNumberOfRows e

postgresUpdatePasswordById
  :: DB.Handle
  -> Id User
  -> EncryptedPassword
  -> ExceptT UserRepositoryError IO NoContent
postgresUpdatePasswordById handle userId password = do
  eitherUser <- runRepositoryQuery handle (Query.selectUserById userId)
  case eitherUser of
    Right _ -> do
      runRepositoryQuery handle (Query.updatePasswordById userId password)
      pure NoContent
    Left e -> throwE $ UnexpectedNumberOfRows e

postgresUpdateUsernameById
  :: DB.Handle -> Id User -> Text -> ExceptT UserRepositoryError IO NoContent
postgresUpdateUsernameById handle userId name = do
  eitherUser <- runRepositoryQuery handle (Query.selectUserById userId)
  case eitherUser of
    Right _ -> do
      runRepositoryQuery handle (Query.updateUsernameById userId name)
      pure NoContent
    Left e -> throwE $ UnexpectedNumberOfRows e

postgresGetProfileById :: DB.Handle -> Id User -> ExceptT UserRepositoryError IO Profile
postgresGetProfileById handle userId = do
  eitherProfile <- runRepositoryQuery handle (Query.getUserProfile userId)
  case eitherProfile of
    Right profile -> pure $ unserializeProfile profile
    Left e -> throwE $ UnexpectedNumberOfRows e

postgresUpdateProfileById :: DB.Handle -> Id User -> Profile -> ExceptT UserRepositoryError IO NoContent
postgresUpdateProfileById handle userId profile = do
  eitherUser <- runRepositoryQuery handle (Query.selectUserById userId)
  case eitherUser of
    Right _ -> do
      let query = Query.updateUserProfile userId profile
      runRepositoryQuery handle query
      pure NoContent
    Left e -> throwE $ UnexpectedNumberOfRows e

postgresCreateProfileById :: DB.Handle -> Id User -> Profile -> ExceptT UserRepositoryError IO NoContent
postgresCreateProfileById handle userId profile = do
  eitherUser <- runRepositoryQuery handle (Query.selectUserById userId)
  case eitherUser of
    Right _ -> do
      let query = Query.createUserProfile . litProfile $ serializeProfile userId profile
      runRepositoryQuery handle query
      pure NoContent
    Left e -> throwE $ UnexpectedNumberOfRows e

-- | Run a query transforming a Hasql.QueryError into a UserRepositoryError as appropriate to the
-- domain.
runRepositoryQuery :: DB.Handle -> Session a -> ExceptT UserRepositoryError IO a
runRepositoryQuery handle = withExceptT liftRepositoryError . ExceptT . DB.runQuery handle

liftRepositoryError :: QueryError -> UserRepositoryError
liftRepositoryError queryError@(QueryError _ _ (ResultError (ServerError "23505" message _ _ _)))
  | "users_name_key" `isInfixOf` message = DuplicateUserName queryError
liftRepositoryError queryError = OtherError queryError
