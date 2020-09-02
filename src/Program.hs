module Program where

import Control.Monad.Except
import qualified Data.Map as Map
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
parser = Program <$> parseArgs <*> Stmt.parser

compile :: Text -> Text
compile x = either showText (Text.unlines . fmap (<> ";")) $
  runExcept $ do
    p <- withExcept ParseError $ liftEither $ parse (parser <* eof) "NC" x
    liftEither $ runGenerator emptyEnv $ generate p

generate :: Program -> Gen ()
generate p = do
  genArgs (args p)
  emit ""
  genVars (args p)
  emit ""
  mapM_ Stmt.generate (body p)

parseArgs :: Parser [(Name, Argument)]
parseArgs = option [] $ do
  reserved "Args"
  (parens $ sepBy arg comma) <* some (lexeme (semicolon <|> eol))
  where
    arg = do
      n <- identifier
      t <- optional $ symbol ":" *> Type.parser
      value <- optional $ symbol "=" *> Lit.parser
      desc <- optional $ Text.pack <$> parens (manyTill charLiteral (lookAhead (symbol ")")))
      let t' = fromMaybe Type.Real $ t <|> fmap Lit.type' value
      pure (n, Argument {argType = t', defaultValue = value, description = desc})

genArgs :: [(Name, Argument)] -> Gen ()
genArgs = mapM_ genArg
  where
    genArg (name, arg) = do
      vars <- gets variables
      when (Map.member name vars) $ throwError $ Error
      adr <- nextAddress
      modifyVars $ Map.insert name (Variable {address = adr, type' = argType arg})
      emit $ "H" <> showText adr <> "   =  +000000.0000  ( " <> Text.justifyLeft 43 ' ' name <> ")"

genVars :: [(Name, Argument)] -> Gen ()
genVars as =
  emitsWithFuture
    ( \env ->
        let vars = Map.toList $ Map.difference (variables env) $ Map.fromList as
         in flip map vars $ \(name, var) ->
              pure ("H" <> showText (address var) <> "   =  +000000.0000  ( " <> Text.justifyLeft 43 ' ' name <> ")")
    )
