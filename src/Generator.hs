module Generator
  ( Gen,
    Variable (..),
    Name,
    runGenerator,
    emptyEnv,
    emit,
    emits,
    throwError,
    showText,
    addVar,
    getVar,
    addLabel,
    getLabel,
    withLabel,
    nextAddress,
    nextRecordNumber,
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
  deriving (Eq)

instance Show Address where
  show (Address x) = Text.unpack $ Text.justifyRight 3 '0' $ showText x

newtype RecordNumber = RecordNumber Int
  deriving (Eq)

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

nextAddress :: Gen Address
nextAddress = do
  Address a <- gets offsetAddress
  modify $ \e -> e {offsetAddress = Address (1 + a)}
  pure $ Address a

nextRecordNumber :: Gen RecordNumber
nextRecordNumber = do
  RecordNumber n <- gets recordNumber
  modify $ \e -> e {recordNumber = RecordNumber (1 + n)}
  pure $ RecordNumber n

addVar :: Name -> Type -> Gen Variable
addVar n t = do
  vars <- gets variables
  case Map.lookup n vars of
    Nothing -> do
      adr <- nextAddress
      let var = Variable {type' = t, address = adr}
      modify (\e -> e {variables = Map.insert n var vars})
      pure var
    Just var -> if t == type' var then pure var else throwError $ Error

getVar :: Name -> Gen Variable
getVar n = gets variables >>= maybe (throwError Error) pure . Map.lookup n

addLabel :: Name -> Gen RecordNumber
addLabel n = do
  ls <- gets labels
  case Map.lookup n ls of
    Nothing -> do
      rn <- nextRecordNumber
      modify $ \e -> e {labels = Map.insert n rn ls}
      pure rn
    Just _ -> throwError $ Error

getLabel :: Name -> Gen RecordNumber
getLabel n = gets labels >>= maybe (throwError Error) pure . Map.lookup n

emit :: Text -> Gen ()
emit = tell . pure . pure

emits :: [Text] -> Gen ()
emits = mapM_ emit

gets :: (Env -> a) -> Gen a
gets = lift . lift . getsPast

modify :: (Env -> Env) -> Gen ()
modify = lift . lift . modifyForwards

withLabel :: Name -> (Maybe RecordNumber -> Either Error Text) -> Gen ()
withLabel n f = (lift $ lift $ getsFuture labels) >>= tell . pure . f . Map.lookup n
