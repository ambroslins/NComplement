module Generator where

import Control.Applicative ((<|>))
import Control.Monad (when)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Error
import Gen
import qualified Literal as Lit
import qualified Parser
import Replace.Megaparsec (splitCap)
import Syntax
import Type (Type)
import qualified Type

eval :: Expr -> Gen (Type, Text)
eval expr = case expr of
  Lit x -> pure (Lit.type' x, showText x)
  Var name -> do
    var <- gets variables >>= maybe (throwError $ UndefinedVar name) pure . Map.lookup name
    pure (type' var, "H" <> (showText $ address var))
  Ref name -> do
    var <- gets variables >>= maybe (throwError $ UndefinedVar name) pure . Map.lookup name
    pure (type' var, showText $ address var)
  Fun name args ->
    maybe
      (throwError $ UndefinedFun name)
      ($ args)
      $ lookup name functions
  Neg x -> do
    (t, e) <- eval x
    case t of
      Type.Int -> pure (Type.Int, "-" <> e)
      Type.Real -> pure (Type.Real, "-" <> e)
      _ -> throwError $ TypeMismatch t Type.Real
  Add x y -> additive x y "+"
  Sub x y -> additive x y "-"
  Mul x y -> do
    (tx, ex) <- eval x
    (ty, ey) <- eval y
    case (tx, ty) of
      (Type.Int, Type.Int) -> pure (Type.Int, formatInfix ex ey "*")
      (Type.Int, Type.Real) -> pure (Type.Real, formatInfix ex ey "*")
      (Type.Real, Type.Int) -> pure (Type.Real, formatInfix ex ey "*")
      (Type.Real, Type.Real) -> pure (Type.Real, squareBrackets $ ex <> "*" <> ey <> "/1.")
      _ -> throwError $ TypeMismatch tx ty
  Div x y -> do
    (tx, ex) <- eval x
    (ty, ey) <- eval y
    case (tx, ty) of
      (Type.Int, Type.Int) -> pure (Type.Int, formatInfix ex ey "/")
      (Type.Real, Type.Int) -> pure (Type.Real, formatInfix ex ey "/")
      (Type.Real, Type.Real) -> pure (Type.Real, squareBrackets $ ex <> "/" <> ey <> "*1.")
      _ -> throwError $ TypeMismatch tx ty
  Pow n e ->
    eval $
      ( if n < 0
          then Div (Lit $ Lit.Real 1.0)
          else id
      )
        $ case replicate (abs n) e of
          [] -> Lit $ Lit.Int 1
          x : xs -> foldr Mul x xs
  where
    additive x y s = do
      (tx, ex) <- eval x
      (ty, ey) <- eval y
      t <- case (tx, ty) of
        (Type.Int, Type.Int) -> pure Type.Int
        (Type.Real, Type.Real) -> pure Type.Real
        _ -> throwError $ TypeMismatch tx ty
      pure (t, formatInfix ex ey s)
    formatInfix x y s = squareBrackets $ x <> s <> y

squareBrackets :: Text -> Text
squareBrackets x = "[" <> x <> "]"

functions :: [(Name, [Expr] -> Gen (Type, Text))]
functions =
  [ ("sin", included "SIN"),
    ("cos", included "COS"),
    ("tan", included "TAN"),
    ("asin", included "ASIN"),
    ("acos", included "ACOS"),
    ("atan", included "ATAN"),
    ("sqrt", included "SQRT"),
    ("round", included "ROUND"),
    ( "norm",
      \xs -> case map (Pow 2) xs of
        [] -> throwError $ Error
        x : xs' -> eval $ Fun "sqrt" $ [foldl Add x xs']
    )
  ]
  where
    included f = \case
      [x] -> do
        (t, e) <- eval x
        case t of
          Type.Real -> pure $ (Type.Real, f <> squareBrackets e)
          _ -> throwError $ TypeMismatch t Type.Real
      _ -> throwError $ Error

generate :: Statement -> Gen ()
generate stmt = do
  case stmt of
    Assign name expr -> do
      (t, e) <- eval expr
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
    If (lhs, comp, rhs) thens melses -> do
      (tl, el) <- eval lhs
      (tr, er) <- eval rhs
      when (tl /= tr) $ throwError $ Error
      rn1 <- nextRecordNumber
      emit $ "IF " <> el <> showText comp <> er <> " (," <> showText rn1 <> ")"
      generate thens
      case melses of
        Nothing -> emit $ "N" <> showText rn1
        Just elses -> do
          rn2 <- nextRecordNumber
          emits $ ["JUMP" <> showText rn2, "N" <> showText rn1]
          generate elses
          emit $ "N" <> showText rn2
    Scope stmts -> mapM_ generate stmts
    Unsafe x -> do
      u <-
        fmap (Text.concat) $
          mapM (either pure (fmap snd . eval)) $
            splitCap (Parser.reference <|> Parser.variable) x
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
