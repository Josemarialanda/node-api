module UserRepo (Table, repository) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.Map.Lazy
  ( Map
  , assocs
  , delete
  , filter
  , filterWithKey
  , insert
  , size
  , update
  )
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID.V4 (nextRandom)
import GHC.Conc (TVar, atomically, readTVar, readTVarIO, writeTVar)
import Hasql.Session
  ( CommandError (ResultError)
  , QueryError (QueryError)
  , ResultError (ServerError)
  )
import Impl.User.Error (UserRepositoryError (..))
import Infrastructure.Persistence.Queries (WrongNumberOfResults (..))
import MatchOrNot.EncryptedPassword (EncryptedPassword)
import MatchOrNot.Repository.User (UserRepository (..))
import MatchOrNot.Types.Id (Id (Id))
import MatchOrNot.User (User (..))
import PostgreSQL.ErrorCodes (unique_violation)
import Servant (NoContent (NoContent))
import Prelude hiding (filter)

type Table = TVar (Map (Id User) User)

repository :: Table -> UserRepository (ExceptT UserRepositoryError IO)
repository userMap =
  UserRepository
    { findByName = inMemoryGetUserByName userMap
    , findById = inMemoryGetUserById userMap
    , add = inMemoryAddUser userMap
    , deleteUserById = inMemoryDeleteUser userMap
    , changePasswordById = inMemorychangePasswordById userMap
    , changeUsernameById = inMemorychangeUsernameById userMap
    , getProfileById = undefined
    }

inMemoryGetUserByName
  :: Table -> Text -> ExceptT UserRepositoryError IO (Id User, User)
inMemoryGetUserByName userMap name' = do
  users <- liftIO $ readTVarIO userMap
  let usersWithName = filter ((== name') . name) users
  case size usersWithName of
    0 -> throwError $ UnexpectedNumberOfRows NoResults
    1 -> pure . head . assocs $ usersWithName
    _ -> throwError $ UnexpectedNumberOfRows MoreThanOneResult

inMemoryGetUserById
  :: Table -> Id User -> ExceptT UserRepositoryError IO (Id User, User)
inMemoryGetUserById userMap userId = do
  users <- liftIO $ readTVarIO userMap
  let usersWithId = filterWithKey (\k _ -> k == userId) users
  case size usersWithId of
    0 -> throwError $ UnexpectedNumberOfRows NoResults
    1 -> pure . head . assocs $ usersWithId
    _ -> throwError $ UnexpectedNumberOfRows MoreThanOneResult

duplicateNameError :: Text -> UserRepositoryError
duplicateNameError name' =
  DuplicateUserName $
    QueryError
      "insert user"
      []
      ( ResultError $
          ServerError
            unique_violation
            "duplicate key value violates unique constraint"
            (Just $ "Key (name)=(" <> encodeUtf8 name' <> ") already exists")
            Nothing
            Nothing
      )

inMemoryAddUser
  :: Table -> Text -> EncryptedPassword -> ExceptT UserRepositoryError IO (Id User)
inMemoryAddUser userMap name' password' = do
  userId <- Id <$> liftIO nextRandom
  queryError <- liftIO . atomically $ do
    users <- readTVar userMap
    let usersWithName = filter ((== name') . name) users
    if null usersWithName
      then writeTVar userMap (insert userId (User name' password') users) >> pure Nothing
      else pure . Just $ duplicateNameError name'
  case queryError of
    Just qe -> throwError qe
    Nothing -> pure userId

inMemoryDeleteUser
  :: Table -> Id User -> ExceptT UserRepositoryError IO NoContent
inMemoryDeleteUser userMap userId = do
  queryError <- liftIO . atomically $ do
    users <- readTVar userMap
    let usersWithId = filterWithKey (\k _ -> k == userId) users
    case size usersWithId of
      0 -> pure . Just $ UnexpectedNumberOfRows NoResults
      1 -> writeTVar userMap (delete userId users) >> pure Nothing
      _ -> pure . Just $ UnexpectedNumberOfRows MoreThanOneResult
  case queryError of
    Just qe -> throwError qe
    Nothing -> pure NoContent

inMemorychangePasswordById
  :: Table
  -> Id User
  -> EncryptedPassword
  -> ExceptT UserRepositoryError IO NoContent
inMemorychangePasswordById userMap userId newPassword = do
  queryError <- liftIO . atomically $ do
    users <- readTVar userMap
    let usersWithId = filterWithKey (\k _ -> k == userId) users
    case size usersWithId of
      0 -> pure . Just $ UnexpectedNumberOfRows NoResults
      1 ->
        writeTVar
          userMap
          (update (\user -> Just user{password = newPassword}) userId users)
          >> pure Nothing
      _ -> pure . Just $ UnexpectedNumberOfRows MoreThanOneResult
  case queryError of
    Just qe -> throwError qe
    Nothing -> pure NoContent

inMemorychangeUsernameById
  :: Table -> Id User -> Text -> ExceptT UserRepositoryError IO NoContent
inMemorychangeUsernameById userMap userId newName = do
  queryError <- liftIO . atomically $ do
    users <- readTVar userMap
    let usersWithId = filterWithKey (\k _ -> k == userId) users
    case size usersWithId of
      0 -> pure . Just $ UnexpectedNumberOfRows NoResults
      1 ->
        writeTVar userMap (update (\user -> Just user{name = newName}) userId users)
          >> pure Nothing
      _ -> pure . Just $ UnexpectedNumberOfRows MoreThanOneResult
  case queryError of
    Just qe -> throwError qe
    Nothing -> pure NoContent
