module NComplement where

import Control.Exception (throwIO)
import Data.Text (Text, pack)
import qualified Data.Text.IO as Text
import Error
import Gen
import qualified Generator
import qualified NC
import qualified Parser
import Syntax (Program)
import System.FilePath
import Text.Megaparsec (parse, pos1)

runCompiler :: FilePath -> FilePath -> IO ()
runCompiler inFilePath outFilePath = do
  input <- Text.readFile inFilePath
  case compile inFilePath input of
    Left e -> throwIO e
    Right output -> Text.writeFile outFilePath output

repl :: IO ()
repl = Text.interact $ either (pack . show) id . compile "NComplement"

compile :: FilePath -> Text -> Either Error Text
compile inFilePath input =
  case parse Parser.program (takeFileName inFilePath) input of
    Left e -> Left $ ParseError e
    Right ast -> generate input ast

generate :: Text -> Program -> Either Error Text
generate input ast =
  NC.printStmts
    <$> runGenerator env (Generator.program ast)
  where
    env =
      Env
        { indices = Index <$> [100 .. 700],
          locations = Location <$> [0, 10, 9000],
          source = input,
          currentLine = pos1,
          symbols = mempty
        }
