module TestServices where

import           API.AppServices           (connectedAuthenticateUser, connectedContentRepository,
                                            connectedUserRepository, encryptedPasswordManager)
import           API.Types.AppServices     (AppServices (..))

import qualified ContentRepo

import           GHC.Conc                  (newTVarIO)

import           Infrastructure.Logger     as Logger (withHandle)
import           Infrastructure.SystemTime as SystemTime (withHandle)

import           Servant.Auth.Server       (defaultJWTSettings, generateKey)

import qualified UserRepo

testServices :: IO AppServices
testServices = do
  key <- generateKey
  userMap <- newTVarIO mempty
  contentsMap <- newTVarIO mempty
  SystemTime.withHandle $ \timeHandle ->
    Logger.withHandle timeHandle $ \loggerHandle -> do
      let passwordManager' = encryptedPasswordManager loggerHandle $ defaultJWTSettings key
      let userRepository' = UserRepo.repository userMap
      let contentsRepository = ContentRepo.repository contentsMap
      pure $
        AppServices
          { jwtSettings = defaultJWTSettings key
          , passwordManager = passwordManager'
          , contentRepository = connectedContentRepository loggerHandle contentsRepository
          , userRepository = connectedUserRepository loggerHandle userRepository'
          , authenticateUser = connectedAuthenticateUser loggerHandle userRepository' passwordManager'
          }
