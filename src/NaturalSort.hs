module NaturalSort (naturalSortOn) where

import Data.Char (isDigit)
import Data.List (groupBy, sortOn)
import Text.Read (readMaybe)

data Token
  = Character Char
  | Integer Int
  deriving (Show, Eq, Ord)

naturalSortOn :: (a -> String) -> [a] -> [a]
naturalSortOn f = sortOn (toTokens . f)

toTokens :: String -> [Token]
toTokens xs = concatMap convert $ groupBy (\x y -> isDigit x && isDigit y) xs
  where
    convert s = maybe (map Character s) (singleton . Integer) $ readMaybe s
    singleton c = [c]
