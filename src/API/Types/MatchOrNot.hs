module API.Types.MatchOrNot (MatchOrNotAPI (..)) where

import Data.Text (Text)
import GHC.Generics (Generic)
import MatchOrNot.Types.Content (Content)
import MatchOrNot.Types.Id (Id)
import MatchOrNot.Types.Owned (Owned)
import MatchOrNot.Types.Profile (Profile)
import MatchOrNot.Types.Tag (Tag)
import Servant (NoContent, Required)
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
import Prelude hiding (getContents)

-- |
-- The main endpoints of the application API
data MatchOrNotAPI mode = MatchOrNotAPI
  { addContent :: mode :- "add-content" :> ReqBody '[JSON] (Content Tag) :> Post '[JSON] (Id (Content Tag))
  -- ^ Add a new 'Content'
  , getContents :: mode :- "get-contents" :> QueryParams "tag" Tag :> Get '[JSON] [Owned (Content Tag)]
  -- ^ Retrieve all the 'User' 'Content's indexed by the provided 'Tag's
  , deleteUser :: mode :- "user" :> "delete" :> Delete '[JSON] NoContent
  -- ^ Delete the 'User' with the provided 'Id'
  , updatePassword
      :: mode :- "user" :> "update-password" :> QueryParam' '[Required] "password" Text :> Post '[JSON] NoContent
  -- ^ Update the password of the 'User' with the provided 'Id'
  , updateUsername
      :: mode :- "user" :> "update-username" :> QueryParam' '[Required] "username" Text :> Post '[JSON] NoContent
  -- ^ Update the username of the 'User' with the provided 'Id'
  , getProfile :: mode :- "user" :> "profile" :> Get '[JSON] Profile
  -- ^ Retrieve the 'Profile' of the 'User' with the provided 'Id'
  , createProfile :: mode :- "user" :> "create-profile" :> ReqBody '[JSON] Profile :> Post '[JSON] NoContent
  -- ^ Create a new 'Profile' for the 'User' with the provided 'Id'
  , updateProfile :: mode :- "user" :> "update-profile" :> ReqBody '[JSON] Profile :> Post '[JSON] NoContent
  -- ^ Update the 'Profile' of the 'User' with the provided 'Id'
  }
  deriving stock (Generic)
