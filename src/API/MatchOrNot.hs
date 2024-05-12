module API.MatchOrNot (matchOrNotServer) where

import API.Types.MatchOrNot (MatchOrNotAPI (..))
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
    , changePassword = changePasswordEndpoint userRepository passwordManager userId
    , changeUsername = changeUsernameById userRepository userId
    , getProfile = getProfileById userRepository userId
    }

changePasswordEndpoint
  :: UserRepository Handler
  -> PasswordManager Handler
  -> Id User
  -> Text
  -> Handler NoContent
changePasswordEndpoint userRepository passwordManager userId password'
  | password' == "" = throwError err400{errBody = "Password cannot be empty"}
  | otherwise = do
      (_, User{name}) <- findById userRepository userId
      let newCredentials = Credentials name (Password $ encodeUtf8 password')
      hashedPassword <- generatePassword passwordManager newCredentials
      changePasswordById userRepository userId hashedPassword
