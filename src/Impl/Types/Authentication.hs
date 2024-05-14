module Impl.Types.Authentication
  ( Error(..)
  ) where

import Impl.Types.User.Error                    (UserRepositoryError)

import Infrastructure.Types.Persistence.Queries (WrongNumberOfResults)

-- |
-- How 'authenticateUser' can actually fail
data Error
  = -- | the provided 'Credentials' data do not correspond to a unique user
    SelectUserError WrongNumberOfResults
  | -- | the interaction with the database somehow failed
    QueryError UserRepositoryError
  | -- | the password provided in the 'Credentials' data is not correct
    PasswordVerificationFailed
  deriving (Show)
