module Program where

import Control.Monad.Except
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Error
import Generator
import Literal (Literal)
import qualified Literal as Lit
import Parser
import Statement (Statement)
import qualified Statement as Stmt
import Text.Megaparsec (eof, parse)
import Type (Type)
import qualified Type

data Argument = Argument
  { argType :: Type,
    defaultValue :: Maybe Literal,
    description :: Maybe Text
  }

data Program = Program
  { args :: [(Name, Argument)],
    body :: [Statement]
  }

parser :: Parser Program
parser = Program <$> arguments <*> Stmt.parser

compile :: Text -> Text
compile x = either showText (Text.unlines . fmap (<> ";")) $
  runExcept $ do
    ast <- withExcept ParseError $ liftEither $ parse (Stmt.parser <* eof) "NC" x
    liftEither $ runGenerator emptyEnv $ mapM Stmt.generate ast

arguments :: Parser [(Name, Argument)]
arguments = option [] $ do
  reserved "Args"
  parens $ sepBy arg comma
  where
    arg = do
      n <- identifier
      t <- optional $ symbol ":" *> Type.parser
      value <- optional $ symbol "=" *> Lit.parser
      desc <- optional $ Text.pack <$> parens (manyTill charLiteral (lookAhead (symbol ")")))
      let t' = fromMaybe Type.Real $ t <|> fmap Lit.type' value
      pure (n, Argument {argType = t', defaultValue = value, description = desc})
