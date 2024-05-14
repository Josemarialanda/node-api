module UserRepo (Table, repository) where

import Application.Types.User.Error          (UserRepositoryError (..))

import Control.Monad.Except                  (throwError)
import Control.Monad.IO.Class                (liftIO)
import Control.Monad.Trans.Except            (ExceptT)

import Core.Types.EncryptedPassword          (EncryptedPassword)
import Core.Types.Id                         (Id (Id))
import Core.Types.User                       (User (..), UserRepository (..))

import Data.Map.Lazy                         (Map, assocs, delete, filter, filterWithKey, insert,
                                              size, update)
import Data.Text                             (Text)
import Data.Text.Encoding                    (encodeUtf8)
import Data.UUID.V4                          (nextRandom)

import GHC.Conc                              (TVar, atomically, readTVar, readTVarIO, writeTVar)

import Hasql.Session                         (CommandError (ResultError), QueryError (QueryError),
                                              ResultError (ServerError))

import Infrastructure.Types.Database.Queries (WrongNumberOfResults (..))

import PostgreSQL.ErrorCodes                 (unique_violation)

import Prelude                               hiding (filter)

import Servant                               (NoContent (NoContent))

type Table = TVar (Map (Id User) User)

repository :: Table -> UserRepository (ExceptT UserRepositoryError IO)
repository userMap =
  UserRepository
    { findByName = inMemoryGetUserByName userMap
    , findById = inMemoryGetUserById userMap
    , add = inMemoryAddUser userMap
    , deleteUserById = inMemoryDeleteUser userMap
    , updatePasswordById = inMemoryUpdatePasswordById userMap
    , updateUsernameById = inMemoryUpdateUsernameById userMap
    , getProfileById = undefined
    , createProfileById = undefined
    , updateProfileById = undefined
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

inMemoryUpdatePasswordById
  :: Table
  -> Id User
  -> EncryptedPassword
  -> ExceptT UserRepositoryError IO NoContent
inMemoryUpdatePasswordById userMap userId newPassword = do
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

inMemoryUpdateUsernameById
  :: Table -> Id User -> Text -> ExceptT UserRepositoryError IO NoContent
inMemoryUpdateUsernameById userMap userId newName = do
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
