module Statement
  ( Statement (..),
    parser,
    generate,
  )
where

import Code (Code)
import qualified Code
import Control.Monad (when)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Error
import Expression (Expr)
import qualified Expression as Expr
import Generator
import Parser
import Replace.Megaparsec (splitCap)

data Statement
  = Assign Name Expr
  | If (Expr, Comperator, Expr) Statement Statement
  | Scope [Statement]
  | Code Code
  | Unsafe Text
  | Label Name
  | Jump Name
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
    sep = some $ lexeme (semicolon <|> eol)

parseStmt :: Parser Statement
parseStmt =
  choice
    [ ifStatement,
      try label,
      assignment,
      scope,
      Code <$> Code.parser,
      unsafe,
      jump
    ]
  where
    ifStatement = do
      reserved "If"
      lhs <- Expr.parser
      comp <- parseComp
      rhs <- Expr.parser
      sThen <- parseStmt
      sElse <- option (Scope []) $ reserved "Else" *> parseStmt
      pure $ If (lhs, comp, rhs) sThen sElse
    label = Label <$> identifier <* symbol ":"
    assignment = do
      var <- identifier
      reserved "="
      e <- Expr.parser
      pure $ Assign var e
    scope = Scope <$> braces parser
    parseComp = choice [Eq <$ symbol "=", Lt <$ symbol "<", Gt <$ symbol ">"]
    unsafe = Unsafe . Text.pack <$> (symbol "!" *> manyTill charLiteral (lookAhead (semicolon <|> eol)))
    jump = symbol "JUMP" >> Jump <$> identifier

generate :: Statement -> Gen ()
generate stmt = do
  case stmt of
    Assign name expr -> do
      (t, e) <- Expr.eval expr
      var <- do
        vars <- gets variables
        case Map.lookup name vars of
          Nothing -> do
            adr <- nextAddress
            let v = Variable {address = adr, type' = t}
            modifyVars $ Map.insert name v
            pure v
          Just v -> do
            when (type' v /= t) $ throwError $ Error
            pure v
      emit $ "H" <> showText (address var) <> " = " <> e
    If (lhs, comp, rhs) thens elses -> do
      (tl, el) <- Expr.eval lhs
      (tr, er) <- Expr.eval rhs
      when (tl /= tr) $ throwError $ Error
      rnElse <- nextRecordNumber
      rnEnd <- nextRecordNumber
      emit $ "IF " <> el <> showText comp <> er <> " (," <> showText rnElse <> ")"
      generate thens
      emits $ ["JUMP" <> showText rnEnd, "N" <> showText rnElse]
      generate elses
      emit $ "N" <> showText rnEnd
    Scope stmts -> mapM_ generate stmts
    Code c -> Code.generate c
    Unsafe x -> do
      u <-
        fmap (Text.concat) $
          mapM (either pure (fmap snd . Expr.eval)) $
            splitCap (Expr.parseRef <|> Expr.parseVar) x
      emit u
    Label name -> do
      ls <- gets labels
      when (Map.member name ls) $ throwError $ Error
      rn <- nextRecordNumber
      modifyLabels $ Map.insert name rn
      emit $ "N" <> showText rn
    Jump name ->
      emitWithFuture $
        maybe
          (Left $ UndefinedLabel name)
          (Right . ("JUMP" <>) . showText)
          . Map.lookup name
          . labels
