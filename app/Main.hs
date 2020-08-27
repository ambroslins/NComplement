module Main where

import qualified Data.Text.IO as Text
import Program

main :: IO ()
main = Text.interact compile
