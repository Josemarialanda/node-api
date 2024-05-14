module Core.Authentication.Authenticator
  ( hoist
  ) where

import Core.Types.Authentication.Authenticator (Authenticator (..))

-- |
-- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'AuthenticateUser' is operating
hoist :: (forall a. m a -> n a) -> Authenticator m -> Authenticator n
hoist f (Authenticator auth) = Authenticator $ f . auth
