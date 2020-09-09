module Generator where

import Control.Applicative ((<|>))
import Control.Monad (forM, when)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Error
import Gen
import Literal (Literal)
import qualified Literal as Lit
import qualified Parser
import Replace.Megaparsec (splitCap)
import Syntax
import Type (Type)
import qualified Type

program :: Program -> Gen ()
program p = do
  defineArgs (arguments p)
  emit ""
  defineVars
  emit ""
  mapM_ statement (body p)

defineArgs :: [(Name, Argument)] -> Gen ()
defineArgs = mapM_ def
  where
    def (name, arg) = do
      vars <- gets variables
      if name `Map.member` vars
        then throwError $ Error
        else do
          adr <- nextAddress
          let var = Variable {type' = argType arg, address = adr}
          modifyVars $ Map.insert name var
          emit $ definition adr (value arg) (fromMaybe name (description arg))

defineVars :: Gen ()
defineVars = do
  prev <- gets variables
  emitsWithFuture $ \env ->
    map
      (pure . def)
      (Map.toList (Map.difference (variables env) prev))
  where
    def (name, var) = definition (address var) Nothing name

definition :: Address -> Maybe Literal -> Text -> Text
definition adr val desc =
  "H" <> showText adr
    <> "   =  "
    <> sign
    <> num
    <> "  ( "
    <> Text.justifyLeft 43 ' ' desc
    <> ")"
  where
    sign = case val of
      Just (Lit.Real x) | x < 0.0 -> "-"
      Just (Lit.Int x) | x < 0 -> "-"
      _ -> "+"
    num = case val of
      Nothing -> "000000.0000"
      Just (Lit.Real x) ->
        let (int, frac) = Text.breakOn "." (showText (abs x))
         in Text.justifyRight 6 '0' int <> Text.justifyLeft 5 '0' (Text.take 5 frac)
      Just (Lit.Int x) ->
        Text.replicate 6 "0" <> "."
          <> Text.justifyRight 4 '0' (showText (abs x))
      Just (Lit.Bool x) ->
        Text.replicate 6 "0" <> "."
          <> Text.justifyRight 4 '0' (showText (Lit.Bool x))

expr :: Expr -> Gen (Type, Text)
expr = \case
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
    (t, e) <- expr x
    case t of
      Type.Int -> pure (Type.Int, squareBrackets $ "-" <> e)
      Type.Real -> pure (Type.Real, squareBrackets $ "-" <> e)
      _ -> throwError $ TypeMismatch t Type.Real
  Add x y -> additive x y "+"
  Sub x y -> additive x y "-"
  Mul x y -> do
    (tx, ex) <- expr x
    (ty, ey) <- expr y
    case (tx, ty) of
      (Type.Int, Type.Int) -> pure (Type.Int, formatInfix ex ey "*")
      (Type.Int, Type.Real) -> pure (Type.Real, formatInfix ex ey "*")
      (Type.Real, Type.Int) -> pure (Type.Real, formatInfix ex ey "*")
      (Type.Real, Type.Real) -> pure (Type.Real, squareBrackets $ ex <> "*" <> ey <> "/1.")
      _ -> throwError $ TypeMismatch tx ty
  Div x y -> do
    (tx, ex) <- expr x
    (ty, ey) <- expr y
    case (tx, ty) of
      (Type.Int, Type.Int) -> pure (Type.Int, formatInfix ex ey "/")
      (Type.Real, Type.Int) -> pure (Type.Real, formatInfix ex ey "/")
      (Type.Real, Type.Real) -> pure (Type.Real, squareBrackets $ ex <> "/" <> ey <> "*1.")
      _ -> throwError $ TypeMismatch tx ty
  Pow n e ->
    expr $
      ( if n < 0
          then Div (Lit $ Lit.Real 1.0)
          else id
      )
        $ case replicate (abs n) e of
          [] -> Lit $ Lit.Int 1
          x : xs -> foldr Mul x xs
  where
    additive x y s = do
      (tx, ex) <- expr x
      (ty, ey) <- expr y
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
        x : xs' -> expr $ Fun "sqrt" $ [foldl Add x xs']
    )
  ]
  where
    included f = \case
      [x] -> do
        (t, e) <- expr x
        case t of
          Type.Real -> pure $ (Type.Real, f <> squareBrackets e)
          _ -> throwError $ TypeMismatch t Type.Real
      _ -> throwError $ Error

statement :: Statement -> Gen ()
statement stmt = do
  case stmt of
    Assign name x -> do
      (t, e) <- expr x
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
      (tl, el) <- expr lhs
      (tr, er) <- expr rhs
      when (tl /= tr) $ throwError $ Error
      rn1 <- nextRecordNumber
      emit $ "IF " <> el <> showText comp <> er <> " (," <> showText rn1 <> ")"
      statement thens
      case melses of
        Nothing -> emit $ "N" <> showText rn1
        Just elses -> do
          rn2 <- nextRecordNumber
          emits $ ["JUMP" <> showText rn2, "N" <> showText rn1]
          statement elses
          emit $ "N" <> showText rn2
    Scope stmts -> mapM_ statement stmts
    Unsafe x -> do
      u <-
        fmap (Text.concat) $
          mapM (either pure (fmap snd . expr)) $
            splitCap (Parser.reference <|> Parser.variable) x
      emit u
    Label name -> do
      ls <- gets labels
      when (Map.member name ls) $ throwError $ Error
      rn <- nextRecordNumber
      modifyLabels $ Map.insert name rn
      emit $ "N" <> showText rn <> " (" <> name <> ")"
    Jump name ->
      emitWithFuture $
        maybe
          (Left $ UndefinedLabel name)
          (Right . \rn -> "JUMP" <> showText rn <> " (" <> name <> ")")
          . Map.lookup name
          . labels
    Codes cs -> instr cs

instr :: NonEmpty Code -> Gen ()
instr cs = emit =<< Text.intercalate " " <$> instr' (toList cs)

instr' :: [Code] -> Gen [Text]
instr' = \case
  [] -> pure []
  G 0 : xs -> do
    xs' <- forM xs $ \case
      X x -> Text.cons 'X' <$> requireType Type.Real x
      Y y -> Text.cons 'Y' <$> requireType Type.Real y
      Z z -> Text.cons 'Z' <$> requireType Type.Real z
      U u -> Text.cons 'U' <$> requireType Type.Real u
      V v -> Text.cons 'V' <$> requireType Type.Real v
      M 5 -> pure "M05"
      F f -> Text.cons 'F' <$> requireType Type.Real f
      _ -> throwError Error
    pure $ "G00" : xs'
  G 1 : xs -> do
    xs' <- forM xs $ \case
      X x -> Text.cons 'X' <$> requireType Type.Real x
      Y x -> Text.cons 'Y' <$> requireType Type.Real x
      Z x -> Text.cons 'Z' <$> requireType Type.Real x
      _ -> throwError Error
    pure $ "G01" : xs'
  G 4 : xs -> case xs of
    [X x] -> sequence $ [pure "G04", Text.cons 'X' <$> requireType Type.Real x]
    _ -> throwError $ Error
  G g : cs
    | Set.member g modalG ->
      (Text.cons 'G' (Text.justifyRight 2 '0' (showText g)) :)
        <$> (instr' cs)
  _ -> throwError Error
  where
    requireType t x = do
      (t', x') <- expr x
      if t' == t then pure x' else throwError $ TypeMismatch t' t
    modalG =
      Set.fromList $
        concat
          [ [5 .. 9],
            [11, 12],
            [26, 27],
            [28 .. 30],
            [40 .. 42],
            [100 * x + y | x <- [0 .. 9], y <- [54 .. 59]],
            [90, 91],
            [126, 127]
          ]
