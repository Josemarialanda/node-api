module API.AppServices
  ( connectedAuthenticateUser
  , connectedContentRepository
  , connectedUserRepository
  , encryptedPasswordManager
  , start
  ) where

import           API.Types.AppServices                               (AppServices (..))

import qualified Application.Authentication                          as Auth
import qualified Application.Content.Postgres                        as ContentPostgres
import qualified Application.Types.Authentication                    as Auth
import qualified Application.Types.User.Error                        as UserPostgres
import qualified Application.User.Postgres                           as UserPostgres

import           Control.Monad                                       ((<=<))
import           Control.Monad.Except                                (throwError)
import           Control.Monad.IO.Class                              (liftIO)
import           Control.Monad.Trans.Except                          (ExceptT, runExceptT)

import qualified Core.Content                                        as ContentRepository
import qualified Core.Types.Authentication.Authenticator             as Auth
import           Core.Types.Content                                  (ContentRepository)
import           Core.Types.User                                     (UserRepository)
import qualified Core.User                                           as UserRepository

import           Crypto.JOSE.JWK                                     (JWK)

import           Hasql.Session                                       (QueryError)

import qualified Infrastructure.Authentication.PasswordManager       as PasswordManager
import           Infrastructure.Logger                               (logError, logWarning,
                                                                      withContext)
import           Infrastructure.Types.Authentication.PasswordManager (PasswordManager,
                                                                      PasswordManagerError (..))
import qualified Infrastructure.Types.Database                       as DB
import           Infrastructure.Types.Database.Queries               (WrongNumberOfResults (..))
import qualified Infrastructure.Types.Logger                         as Logger

import           Prelude                                             hiding (log)

import           Servant                                             (Handler, err401, err403,
                                                                      err500)
import           Servant.Auth.Server                                 (JWTSettings,
                                                                      defaultJWTSettings)

-- |
-- Lifts a computation from 'ExceptT e IO' to 'Handler a' using the provided 'handleError' function
eitherTToHandler :: (e -> Handler a) -> ExceptT e IO a -> Handler a
eitherTToHandler handleError = either handleError pure <=< liftIO . runExceptT

-- |
-- Lifts a 'ContentRepository' for the 'Handler' monad, handling all errors by logging them and returning a 500 response
connectedContentRepository
  :: Logger.Handle
  -> ContentRepository (ExceptT QueryError IO)
  -> ContentRepository Handler
connectedContentRepository logHandle =
  ContentRepository.hoist
    (eitherTToHandler $ (>> throwError err500) . logError logHandle . show)

-- |
-- Lifts a 'UserRepository' for the 'Handler' monad, handling all errors by logging them and returning a 500 response
connectedUserRepository
  :: Logger.Handle
  -> UserRepository (ExceptT UserPostgres.UserRepositoryError IO)
  -> UserRepository Handler
connectedUserRepository logHandle = UserRepository.hoist $ eitherTToHandler handleUserRepositoryError
  where
    handleUserRepositoryError :: UserPostgres.UserRepositoryError -> Handler a
    -- If the database error concerns a duplicate user, we return a 403 response
    handleUserRepositoryError (UserPostgres.DuplicateUserName e) = do
      logWarning logHandle $ show (UserPostgres.DuplicateUserName e)
      throwError err403
    -- Otherwise, we return a 500 response
    handleUserRepositoryError e = do
      logError logHandle (show e)
      throwError err500

-- |
-- Creates an 'AuthenticateUser' service injecting its dependencies and handling errors
connectedAuthenticateUser
  :: Logger.Handle
  -> UserRepository (ExceptT UserPostgres.UserRepositoryError IO)
  -> PasswordManager Handler
  -> Auth.Authenticator Handler
connectedAuthenticateUser logHandle userRepository' passwordManager' =
  Auth.hoist
    (eitherTToHandler handleAuthenticationError)
    (Auth.authenticator userRepository' passwordManager')
  where
    handleAuthenticationError :: Auth.Error -> Handler a
    -- If the user was not found, we return a 401 response
    handleAuthenticationError (Auth.QueryError (UserPostgres.UnexpectedNumberOfRows NoResults)) = do
      throwError err401
    -- If there was an error at the database level, we return a 500 response
    handleAuthenticationError (Auth.QueryError e) = do
      logError logHandle $ show (Auth.QueryError e)
      throwError err500
    -- In other cases, there was an authentication error and we return a 401 response
    handleAuthenticationError e = do
      logWarning logHandle (show e)
      throwError err401

-- |
-- Creates a 'PasswordManager' service injecting its dependencies and handling errors
encryptedPasswordManager
  :: Logger.Handle -> JWTSettings -> PasswordManager Handler
encryptedPasswordManager logHandle =
  PasswordManager.hoist (eitherTToHandler handlePasswordManagerError)
    . PasswordManager.bcryptPasswordManager
  where
    handlePasswordManagerError :: PasswordManagerError -> Handler a
    -- If there was a failure during password hashing, we return a 500 response
    handlePasswordManagerError FailedHashing = do
      logError logHandle $ show FailedHashing
      throwError err500
    -- In other cases, we return a 401 response
    handlePasswordManagerError (FailedJWTCreation e) = do
      logError logHandle $ show (FailedJWTCreation e)
      throwError err401

start :: DB.Handle -> Logger.Handle -> JWK -> AppServices
start dbHandle logHandle key =
  AppServices
    { jwtSettings = defaultJWTSettings key
    , passwordManager = passwordManager'
    , contentRepository = connectedContentRepository (logContext "ContentRepository") dbContentRepository
    , userRepository = connectedUserRepository (logContext "UserRepository") dbUserRepository
    , authenticateUser = connectedAuthenticateUser (logContext "AuthenticateUser") dbUserRepository passwordManager'
    }
  where
    logContext = flip withContext logHandle
    passwordManager' = encryptedPasswordManager (withContext "PasswordManager" logHandle) $ defaultJWTSettings key
    dbUserRepository = UserPostgres.repository dbHandle
    dbContentRepository = ContentPostgres.repository dbHandle
