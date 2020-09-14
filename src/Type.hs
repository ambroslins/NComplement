module Type
  ( Type (..),
  )
where

data Type
  = Bool
  | Int
  | Real
  | Index
  | Location
  | Function
  | String
  deriving (Eq, Show)
