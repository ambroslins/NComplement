module Main where

import Data.Bifunctor (first)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Error
import Gen
import qualified NC
import qualified Generator
import qualified Parser
import Text.Megaparsec (parse)

main :: IO ()
main = Text.interact $ \input -> either (Text.pack . show) NC.printStmts $ do
  ast <- first ParseError $ parse Parser.program "NC" input
  runGenerator emptyEnv $ Generator.program ast
