module Gen
  ( Gen,
    Env (variables, labels),
    Variable (..),
    Index,
    RecordNumber,
    runGenerator,
    emptyEnv,
    get,
    gets,
    modify,
    modifyVars,
    modifyLabels,
    emit,
    emits,
    emitWithFuture,
    emitsWithFuture,
    nextIndex,
    nextRecordNumber,
    throwError,
  )
where

import Control.Monad.Except
import Control.Monad.Tardis
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map
import Error
import NC (Index (..), RecordNumber (..), Statement)
import Syntax (Name)
import Type (Type)

type Gen = WriterT [Either Error Statement] (ExceptT Error (Tardis Env Env))

data Env = Env
  { offsetIndex :: Index,
    recordNumber :: RecordNumber,
    variables :: Map Name Variable,
    labels :: Map Name RecordNumber
  }

data Variable = Variable
  { index :: Index,
    type' :: Type
  }

runGenerator :: Env -> Gen a -> Either Error [Statement]
runGenerator env = join . fmap foldEither . flip evalTardis (undefined, env) . runExceptT . execWriterT . revert
  where
    foldEither [] = Right []
    foldEither (Left x : _) = Left x
    foldEither (Right x : xs) = (x :) <$> foldEither xs
    revert = (>> (lift $ lift $ getPast >>= sendPast))

emptyEnv :: Env
emptyEnv =
  Env
    { offsetIndex = Index 0,
      recordNumber = RecordNumber 0,
      variables = Map.empty,
      labels = Map.empty
    }

get :: Gen Env
get = gets id

gets :: (Env -> a) -> Gen a
gets = lift . lift . getsPast

modify :: (Env -> Env) -> Gen ()
modify = lift . lift . modifyForwards

modifyVars :: (Map Name Variable -> Map Name Variable) -> Gen ()
modifyVars f = modify $ \env -> env {variables = f (variables env)}

modifyLabels :: (Map Name RecordNumber -> Map Name RecordNumber) -> Gen ()
modifyLabels f = modify $ \env -> env {labels = f (labels env)}

emit :: Statement -> Gen ()
emit = tell . pure . pure

emits :: [Statement] -> Gen ()
emits = mapM_ emit

emitWithFuture :: (Env -> Either Error Statement) -> Gen ()
emitWithFuture = emitsWithFuture . fmap pure

emitsWithFuture :: (Env -> [Either Error Statement]) -> Gen ()
emitsWithFuture f = (lift $ lift $ getFuture) >>= tell . f

nextIndex :: Gen Index
nextIndex = do
  Index i <- gets offsetIndex
  modify $ \env -> env {offsetIndex = Index (1 + i)}
  pure $ Index i

nextRecordNumber :: Gen RecordNumber
nextRecordNumber = do
  RecordNumber n <- gets recordNumber
  modify $ \env -> env {recordNumber = RecordNumber (1 + n)}
  pure $ RecordNumber n
