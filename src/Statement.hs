module Statement
  ( Statement (..),
    parser,
    generate,
  )
where

import Code (Code)
import qualified Code
import Generator
import Control.Monad (void, when)
import qualified Data.Text as Text
import Expression (Expr)
import qualified Expression as Expr
import Parser
import Replace.Megaparsec (splitCap)

data Statement
  = Assign Name Expr
  | If (Expr, Comperator, Expr) Statement Statement
  | Scope [Statement]
  | Code Code
  | Unsafe Text
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
parseStmt = choice [ifStatement, assignment, scope, Code <$> Code.parser, unsafe]
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
    unsafe = Unsafe . Text.pack <$> (symbol "!" *> manyTill charLiteral (lookAhead (void semicolon <|> void newline)))

generate :: Statement -> Generator [Text]
generate stmt = do
  case stmt of
    Assign name expr -> do
      (t, e) <- Expr.generate expr
      var <- insertVar name t
      pure $ ["H" <> showText (address var) <> " = " <> e]
    If (lhs, comp, rhs) thens elses -> do
      (tl, el) <- Expr.generate lhs
      (tr, er) <- Expr.generate rhs
      when (tl /= tr) $ throwError $ Error
      rnElse <- nextRecordNumber
      rnEnd <- nextRecordNumber
      concat
        <$> sequence
          [ pure ["IF " <> el <> showText comp <> er <> " (," <> showText rnElse <> ")"],
            generate thens,
            pure ["JUMP" <> showText rnEnd, "N" <> showText rnElse],
            generate elses,
            pure ["N" <> showText rnEnd]
          ]
    Scope stmts -> (fmap ("  " <>) . concat) <$> mapM generate stmts
    Code c -> Code.generate c
    Unsafe x ->
      fmap (pure . Text.concat) $
        mapM
          (either pure (fmap snd . Expr.generate))
          $ splitCap (Expr.parseRef <|> Expr.parseVar) x
