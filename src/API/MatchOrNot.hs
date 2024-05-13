module API.MatchOrNot (module API.Types.MatchOrNot, matchOrNotServer) where

import API.Types.MatchOrNot
import Control.Monad.Except (throwError)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Infrastructure.Types.Authentication.PasswordManager
  ( PasswordManager (generatePassword)
  )
import MatchOrNot.Authentication.Credentials (Credentials (..), Password (..))
import MatchOrNot.Content
  ( ContentRepository (addContentWithTags, selectUserContentsByTags)
  )
import MatchOrNot.Types.Id (Id)
import MatchOrNot.Types.User (User (..), UserRepository (..))
import Servant (Handler, NoContent, err400)
import Servant.Server (ServerError (..))
import Servant.Server.Generic (AsServer)
import Prelude hiding (getContents)

matchOrNotServer
  :: Id User
  -> PasswordManager Handler
  -> UserRepository Handler
  -> ContentRepository Handler
  -> MatchOrNotAPI AsServer
matchOrNotServer userId passwordManager userRepository contentRepository =
  MatchOrNotAPI
    { addContent = addContentWithTags contentRepository userId
    , getContents = selectUserContentsByTags contentRepository userId
    , deleteUser = deleteUserById userRepository userId
    , updatePassword = updatePasswordEndpoint userRepository passwordManager userId
    , updateUsername = updateUsernameById userRepository userId
    , getProfile = getProfileById userRepository userId
    , createProfile = createProfileById userRepository userId
    , updateProfile = updateProfileById userRepository userId
    }

updatePasswordEndpoint
  :: UserRepository Handler
  -> PasswordManager Handler
  -> Id User
  -> Text
  -> Handler NoContent
updatePasswordEndpoint userRepository passwordManager userId password'
  | password' == "" = throwError err400{errBody = "Password cannot be empty"}
  | otherwise = do
      (_, User{name}) <- findById userRepository userId
      let newCredentials = Credentials name (Password $ encodeUtf8 password')
      hashedPassword <- generatePassword passwordManager newCredentials
      updatePasswordById userRepository userId hashedPassword
