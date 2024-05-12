module Infrastructure.Types.Persistence.Queries (WrongNumberOfResults (..)) where

-- |
-- Describes the possible error cases for queries that expect exactly one row as a result.
data WrongNumberOfResults
  = NoResults
  | MoreThanOneResult
  deriving (Show)
