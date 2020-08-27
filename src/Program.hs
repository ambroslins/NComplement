module Program where

import Compiler
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Statement as Stmt
import Text.Megaparsec (eof, parse)

compile :: Text -> Text
compile x = either showText (Text.unlines . fmap (<> ";")) $
  runExcept $ do
    ast <- withExcept ParseError $ liftEither $ parse (Stmt.parser <* eof) "NC" x
    liftEither $ runCompiler $ concat <$> mapM Stmt.compile ast
