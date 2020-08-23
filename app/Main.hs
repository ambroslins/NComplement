module Main where

import Compile (runCompiler)

main :: IO ()
main = interact runCompiler
