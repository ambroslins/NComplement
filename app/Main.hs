module Main where

import Compile (runCompiler)
import qualified Data.Text.IO as Text

main :: IO ()
main = Text.interact runCompiler
