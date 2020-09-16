module Main where

import Control.Exception
import Data.Char (toUpper)
import Error
import NComplement
import System.Environment
import System.Exit
import System.FilePath

main :: IO ()
main =
  handle handler $
    getArgs >>= \case
      [] -> repl
      [inFilePath] ->
        if ext == ".nco"
          then runCompiler inFilePath outFilePath
          else throwIO $ InvalidExtension ext
        where
          (file, ext) = splitExtension inFilePath
          (path, name) = splitFileName file
          outFilePath = path </> map toUpper name <.> ".NC"
      _ -> throwIO InvalidArguments
  where
    handler :: SomeException -> IO ()
    handler e = print e >> exitFailure
