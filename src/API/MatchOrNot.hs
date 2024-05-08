module API.MatchOrNot where

import Control.Monad.Except (throwError)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Infrastructure.Authentication.PasswordManager
  ( PasswordManager (generatePassword)
  )
import MatchOrNot.Authentication.Credentials
  ( Credentials (Credentials)
  , Password (Password)
  )
import MatchOrNot.Content (Content)
import MatchOrNot.Id (Id)
import MatchOrNot.Owned (Owned)
import MatchOrNot.Repository.Content
  ( ContentRepository (addContentWithTags, selectUserContentsByTags)
  )
import MatchOrNot.Repository.User (UserRepository (..))
import MatchOrNot.Tag (Tag)
import MatchOrNot.User (User (..))
import Servant (Handler, NoContent, Required, err400)
import Servant.API
  ( Delete
  , Get
  , JSON
  , Post
  , QueryParam'
  , QueryParams
  , ReqBody
  , type (:>)
  )
import Servant.API.Generic ((:-))
import Servant.Server (ServerError (..))
import Servant.Server.Generic (AsServer)
import Prelude hiding (getContents)

-- |
-- The main endpoints of the application API
data MatchOrNotAPI mode = MatchOrNotAPI
  { addContent
      :: mode
        :- "add-content"
          :> ReqBody '[JSON] (Content Tag)
          :> Post '[JSON] (Id (Content Tag))
  -- ^ Add a new 'Content'
  , getContents
      :: mode
        :- "get-contents" :> QueryParams "tag" Tag :> Get '[JSON] [Owned (Content Tag)]
  -- ^ Retrieve all the 'User' 'Content's indexed by the provided 'Tag's
  , deleteUser :: mode :- "user" :> "delete" :> Delete '[JSON] NoContent
  -- ^ Change the 'Credentials' of the 'User' with the provided 'Id'
  , changePassword
      :: mode
        :- "user"
          :> "change-password"
          :> QueryParam' '[Required] "password" Text
          :> Post '[JSON] NoContent
  -- ^ Change the password of the 'User' with the provided 'Id'
  , changeUsername
      :: mode
        :- "user"
          :> "change-username"
          :> QueryParam' '[Required] "username" Text
          :> Post '[JSON] NoContent
  -- ^ Change the username of the 'User' with the provided 'Id'
  }
  deriving stock (Generic)

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
    }

changePasswordEndpoint
  :: UserRepository Handler
  -> PasswordManager Handler
  -> Id User
  -> Text
  -> Handler NoContent
changePasswordEndpoint userRepository passwordManager userId password' = do
  (_, User{name}) <- findById userRepository userId
  if password' == ""
    then throwError err400{errBody = "Password cannot be empty"}
    else do
      let newCredentials = Credentials name (Password $ encodeUtf8 password')
      hashedPassword <- generatePassword passwordManager newCredentials
      changePasswordById userRepository userId hashedPassword
