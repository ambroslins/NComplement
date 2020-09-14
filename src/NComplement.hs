module NComplement where

import Control.Exception (throwIO)
import qualified Data.Text.IO as Text
import Error
import Gen
import qualified Generator
import qualified NC
import qualified Parser
import System.FilePath
import Text.Megaparsec (parse)

compile :: FilePath -> FilePath -> IO ()
compile inFilePath outFilePath = do
  input <- Text.readFile inFilePath
  case parse Parser.program (takeFileName inFilePath) input of
    Left e -> throwIO $ ParseError e
    Right ast -> case runGenerator emptyEnv (Generator.program ast) of
      Left e -> throwIO $ e
      Right output -> Text.writeFile outFilePath (NC.printStmts output)
