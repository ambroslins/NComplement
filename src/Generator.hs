module Generator where

import Control.Monad (forM, when)
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Error
import Gen
import Literal (Literal)
import qualified Literal as Lit
import Located
import qualified NC
import Prettyprinter
import Result
import Syntax
import Type (Type)
import qualified Type

program :: Program -> Gen ()
program p = do
  defineArgs (arguments p)
  defineVars
  mapM_ statement (body p)

digit :: Int
digit = 1

eol :: Doc'
eol = semi <> hardline

definition :: Index -> Maybe (Sign, Literal) -> Text -> Doc'
definition i def desc =
  "H" <> pretty i <> "   =  " <> pretty (value def) <> "   "
    <> parens
      ( column $
          \c -> pretty $ Text.justifyLeft (71 - c) ' ' (Text.toUpper desc)
      )
    <> eol
  where
    showAbs :: (Show a, Integral a) => a -> Text
    showAbs = Text.pack . show . abs
    sign Plus = "+"
    sign Minus = "+"
    value = \case
      Just (s, Lit.Real x) ->
        let (int, frac) = Text.breakOn "." (showAbs x)
         in sign s <> Text.justifyRight (6 - digit) '0' int
              <> Text.take (4 + digit) (Text.justifyLeft (4 + digit) '0' frac)
      Just (s, Lit.Int x) ->
        sign s <> Text.replicate (6 - digit) "0" <> "."
          <> Text.justifyRight (3 + digit) '0' (showAbs x)
      Just (_, Lit.Bool x) ->
        value $ Just (Plus, Lit.Int $ if x then 1 else 0)
      _ -> value $ Just (Plus, Lit.Int 0)

defineArgs :: [(Name, Argument)] -> Gen ()
defineArgs = mapM_ def
  where
    def (name, arg) = do
      vars <- gets symbols
      if name `Map.member` vars
        then throwE $ AlreadyDefined name
        else do
          i <- nextIndex
          let t = either id (Lit.type' . snd) $ typeOrDefault arg
          let var = Variable {typeof = t, index = i}
          modifySymbols $ Map.insert name (Var var)
          emit $
            definition
              i
              (either (const Nothing) (Just) $ typeOrDefault arg)
              (fromMaybe (unName name) (description arg))

defineVars :: Gen ()
defineVars = do
  prev <- gets symbols
  emitWithFuture $ \env ->
    pure $
      vsep $
        mapMaybe
          ( \case
              (name, Var var) -> Just $ definition (index var) Nothing (unName name)
              _ -> Nothing
          )
          (Map.toList (Map.difference (symbols env) prev))

expr :: Expr -> Gen (Type, Doc')
expr = \case
  Lit x -> pure (Lit.type' x, pretty x)
  Sym name -> do
    syms <- gets symbols
    case Map.lookup name syms of
      Just (Var var) -> pure (typeof var, "H" <> pretty (index var))
      Just _ -> throwE $ NotAVariable name
      Nothing -> throwE $ UndefinedSymbol name
  Ref name -> do
    syms <- gets symbols
    case Map.lookup name syms of
      Just (Var var) -> pure (Type.Index, pretty (index var))
      Just _ -> throwE $ NotAVariable name
      Nothing -> throwE $ UndefinedSymbol name
  App name args -> do
    syms <- gets symbols
    case Map.lookup name syms of
      Just (Fun f) -> f args
      Just _ -> throwE $ NotAFunction name
      Nothing -> throwE $ UndefinedSymbol name
  Ret _ -> pure (Type.Real, "HRET")
  Neg x -> do
    (t, x') <- expr x
    let x'' = "-" <> brackets x'
    case t of
      Type.Int -> pure (Type.Int, x'')
      Type.Real -> pure (Type.Real, x'')
      _ -> throwE $ TypeMismatchNeg t
  Add x y -> do
    (tx, x') <- expr x
    (ty, y') <- expr y
    let z = brackets (x' <> "+" <> y')
    case (tx, ty) of
      (Type.Real, Type.Real) -> pure (Type.Real, z)
      (Type.Int, Type.Int) -> pure (Type.Int, z)
      _ -> throwE $ TypeMismatchAdd tx ty
  Sub x y -> do
    (tx, x') <- expr x
    (ty, y') <- expr y
    let z = brackets (x' <> "-" <> y')
    case (tx, ty) of
      (Type.Real, Type.Real) -> pure (Type.Real, z)
      (Type.Int, Type.Int) -> pure (Type.Int, z)
      _ -> throwE $ TypeMismatchAdd tx ty
  Mul x y -> do
    (tx, x') <- expr x
    (ty, y') <- expr y
    let z = brackets (x' <> "*" <> y')
    case (tx, ty) of
      (Type.Int, Type.Int) -> pure (Type.Int, z)
      (Type.Int, Type.Real) -> pure (Type.Real, z)
      (Type.Real, Type.Int) -> pure (Type.Real, z)
      (Type.Real, Type.Real) -> pure (Type.Real, brackets (x' <> "*" <> y' <> "/1."))
      _ -> throwE $ TypeMismatchAdd tx ty
  Div x y -> do
    (tx, x') <- expr x
    (ty, y') <- expr y
    let z = brackets (x' <> "/" <> y')
    case (tx, ty) of
      (Type.Int, Type.Int) -> pure (Type.Int, z)
      (Type.Real, Type.Int) -> pure (Type.Real, z)
      (Type.Real, Type.Real) -> pure (Type.Real, brackets (x' <> "/" <> y' <> "*1."))
      _ -> throwE $ TypeMismatchDiv tx ty
  Pow n x ->
    expr $
      ( if n < 0
          then Div (Lit $ Lit.Real 1.0)
          else id
      )
        $ case replicate (abs n) x of
          [] -> Lit $ Lit.Int 1
          x' : xs -> foldr Mul x' xs

functions :: [(Name, [Expr] -> Gen (Type, Doc'))]
functions =
  [ included "sin",
    included "cos",
    included "tan",
    included "asin",
    included "acos",
    included "atan",
    included "sqrt",
    included "round",
    ( "norm",
      \args -> case map (Pow 2) args of
        [] -> throwE $ TypeMismatchApp "norm" [Type.Real, Type.Real] []
        x : xs -> expr $ App "sqrt" $ [foldl Add x xs]
    )
  ]
  where
    included name =
      ( name,
        \case
          [x] -> do
            (t, x') <- mapDoc brackets $ expr x
            if t == Type.Real
              then pure (Type.Real, pretty name <> parens x')
              else throwE $ TypeMismatchApp name [Type.Real] [t]
          _ -> throwE $ TypeMismatchApp name [Type.Real] []
      )

statement :: Statement -> Gen ()
statement (At pos stmt) = do
  setSourceLine pos
  case stmt of
    Assign name x -> do
      (t, x') <- expr x
      var <- defineVar name t
      emit $ "H" <> pretty (index var) <+> "=" <+> x' <> eol
    If (l, ord, r) thens melses -> do
      (tl, xl) <- expr l
      (tr, xr) <- expr r
      when (tl /= tr) $ throwE $ TypeMismatchIf tl tr
      l1 <- nextLocation
      let s = case ord of
            EQ -> "="
            LT -> "<"
            GT -> ">"
      emit $ "IF" <+> xl <> s <> xr <+> tupled ["", pretty l1] <> eol
      statement thens
      case melses of
        Nothing -> emit $ "N" <> pretty l1
        Just elses -> do
          l2 <- nextLocation
          emit $ "JUMP" <> pretty l2 <> eol
          emit $ "N" <> pretty l1 <> eol
          statement elses
          emit $ "N" <> pretty l2 <> eol
    Scope stmts -> mapM_ statement stmts
    Unsafe x -> do
      xs <- mapM (either pure (fmap (NC.toText . snd) . expr)) x
      emit $ pretty $ Text.concat xs
    Label name -> do
      syms <- gets symbols
      if name `Map.member` syms
        then throwE $ AlreadyDefined name
        else do
          loc <- nextLocation
          modifySymbols $ Map.insert name (Loc loc)
          emit $ "N" <> pretty loc <+> parens (pretty name)
    Codes cs -> do
      syms <- gets symbols
      xs <- forM (toList cs) $ \(Code adr x) -> case x of
        Sym name | not (name `Map.member` syms) -> pure $ Left (adr, name)
        x' -> (Right . (pretty adr <>) . snd <$> expr x')
      emitWithFuture $ \env ->
        hsep
          <$> fromEither
            ( forM
                xs
                ( \case
                    Right x -> Right x
                    Left (adr, name) -> case Map.lookup name (symbols env) of
                      Just (Loc loc) -> pure $ pretty adr <> pretty loc
                      _ -> Left $ UndefinedSymbol name
                )
            )
    Call adr xs -> emit =<< ((pretty adr <>) . tupled . fmap snd <$> mapM expr xs)
    Get names address -> do
      xs <- f (toList names) (toList address)
      emit $ hsep ("G83" : xs)
      where
        f [] [] = pure []
        f (n : ns) (a : as) = case Map.lookup a getters of
          Nothing -> throwE $ NotAGetter a
          Just t -> do
            var <- defineVar n t
            (pretty a <> pretty (index var) :) <$> f ns as
        f _ _ = throwE GetSetNotMatching
        getters =
          Map.fromList $
            [ ("X", Type.Real),
              ("Y", Type.Real),
              ("Z", Type.Real),
              ("U", Type.Real),
              ("V", Type.Real),
              ("I", Type.Int),
              ("E", Type.Int)
            ]
    Set address exprs -> do
      xs <- f (toList address) (toList exprs)
      emit $ hsep ("G92" : xs)
      where
        f [] [] = pure []
        f (a : as) (e : es) = case Map.lookup a setters of
          Nothing -> throwE $ NotASetter a
          Just t -> do
            (t', e') <- expr e
            if t == t'
              then (pretty a <> e' :) <$> f as es
              else throwE $ TypeMismatchSet a t t'
        f _ _ = throwE GetSetNotMatching
        setters =
          Map.fromList $
            [ ("X", Type.Real),
              ("Y", Type.Real),
              ("Z", Type.Real),
              ("U", Type.Real),
              ("V", Type.Real)
            ]
  where
    defineVar name t = do
      syms <- gets symbols
      case Map.lookup name syms of
        Nothing -> do
          i <- nextIndex
          let var = Variable {typeof = t, index = i}
          modifySymbols $ Map.insert name (Var var)
          pure var
        Just (Var var) -> do
          let t' = typeof var
          if t' == t
            then pure var
            else throwE $ TypeMismatchDef name t' t
        Just _ -> throwE $ NotAVariable name
