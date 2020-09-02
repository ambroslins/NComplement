module Generator
  ( Gen,
    Env (variables, labels),
    Variable (..),
    Name,
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
    nextAddress,
    nextRecordNumber,
    throwError,
    showText,
  )
where

import Control.Monad.Except
import Control.Monad.Tardis
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Error
import Type (Type)

type Gen = WriterT [Either Error Text] (ExceptT Error (Tardis Env Env))

showText :: Show a => a -> Text
showText = Text.pack . show

newtype Address = Address Int
  deriving (Eq, Ord)

instance Show Address where
  show (Address x) = Text.unpack $ Text.justifyRight 3 '0' $ showText x

newtype RecordNumber = RecordNumber Int
  deriving (Eq, Ord)

instance Show RecordNumber where
  show (RecordNumber x) = Text.unpack $ Text.justifyRight 4 '0' $ showText x

type Name = Text

data Env = Env
  { offsetAddress :: Address,
    recordNumber :: RecordNumber,
    variables :: Map Name Variable,
    labels :: Map Name RecordNumber
  }

data Variable = Variable
  { address :: Address,
    type' :: Type
  }

runGenerator :: Env -> Gen a -> Either Error [Text]
runGenerator env = join . fmap foldEither . flip evalTardis (undefined, env) . runExceptT . execWriterT . revert
  where
    foldEither [] = Right []
    foldEither (Left x : _) = Left x
    foldEither (Right x : xs) = (x :) <$> foldEither xs
    revert = (>> (lift $ lift $ getPast >>= sendPast))

emptyEnv :: Env
emptyEnv =
  Env
    { offsetAddress = Address 0,
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

emit :: Text -> Gen ()
emit = tell . pure . pure

emits :: [Text] -> Gen ()
emits = mapM_ emit

emitWithFuture :: (Env -> Either Error Text) -> Gen ()
emitWithFuture = emitsWithFuture . fmap pure

emitsWithFuture :: (Env -> [Either Error Text]) -> Gen ()
emitsWithFuture f = (lift $ lift $ getFuture) >>= tell . f

nextAddress :: Gen Address
nextAddress = do
  Address a <- gets offsetAddress
  modify $ \env -> env {offsetAddress = Address (1 + a)}
  pure $ Address a

nextRecordNumber :: Gen RecordNumber
nextRecordNumber = do
  RecordNumber n <- gets recordNumber
  modify $ \env -> env {recordNumber = RecordNumber (1 + n)}
  pure $ RecordNumber n
