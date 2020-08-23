module Main where

import Control.Monad (forever)
import Parse (statements)
import Text.Megaparsec (parseTest)

main :: IO ()
main = forever $ getLine >>= parseTest statements
