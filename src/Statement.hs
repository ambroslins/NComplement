module Statement
  ( Statement (..),
    parser,
    compile,
  )
where

import Code (Code)
import qualified Code
import Compiler
import Control.Monad (void, when)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as Text
import Expression (Expr, Name)
import qualified Expression as Expr
import Parser
import qualified Type

data Statement
  = Assign Name Expr
  | If (Expr, Comperator, Expr) Statement Statement
  | Scope [Statement]
  | Code Code
  deriving (Eq, Show)

data Comperator
  = Eq
  | Lt
  | Gt
  deriving (Eq)

instance Show Comperator where
  show = \case
    Eq -> "="
    Lt -> "<"
    Gt -> "<"

parser :: Parser [Statement]
parser = sepEndBy parseStmt sep
  where
    sep = lexeme (void semicolon <|> void newline) >> many (lexeme newline)

parseStmt :: Parser Statement
parseStmt = choice [ifStatement, assignment, scope, Code <$> Code.parser]
  where
    ifStatement = do
      reserved "If"
      lhs <- Expr.parser
      comp <- parseComp
      rhs <- Expr.parser
      sThen <- parseStmt
      sElse <- option (Scope []) $ reserved "Else" *> parseStmt
      pure $ If (lhs, comp, rhs) sThen sElse
    assignment = do
      var <- identifier
      reserved "="
      e <- Expr.parser
      pure $ Assign var e
    scope = Scope <$> braces parser
    parseComp = choice [Eq <$ symbol "=", Lt <$ symbol "<", Gt <$ symbol ">"]

compile :: Statement -> Compiler [Text]
compile stmt = do
  case stmt of
    Assign name expr -> do
      (t, e) <- Expr.compile expr
      var <- insertVar name t
      pure $ ["H" <> showText (address var) <> " = " <> e]
    If (lhs, comp, rhs) thens elses -> do
      (tl, el) <- Expr.compile lhs
      (tr, er) <- Expr.compile rhs
      when (tl /= tr) $ throwError $ Error
      rnElse <- nextRecordNumber
      rnEnd <- nextRecordNumber
      concat
        <$> sequence
          [ pure ["IF " <> el <> showText comp <> er <> " (," <> showText rnElse <> ")"],
            compile thens,
            pure ["JUMP" <> showText rnEnd, "N" <> showText rnElse],
            compile elses,
            pure ["N" <> showText rnEnd]
          ]
    Scope stmts -> (fmap ("  " <>) . concat) <$> mapM compile stmts
    Code c -> case c of
      Code.G00 xs -> do
        let f (a, expr) = do
              (t, e) <- Expr.compile expr
              if t == Type.Real then pure $ showText a <> e else throwError $ Error
        as <- mapM f $ toList xs
        pure $ pure $ "G00 " <> Text.intercalate " " as
