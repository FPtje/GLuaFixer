module Main where

import GLua.ASTInstances ()

import GHC.IO.Encoding (setLocaleEncoding, utf8)

import Effectful (runEff)
import GLuaFixer.Effects.Run (run)
import qualified Effectful.Environment as Env
import System.Exit (exitWith)
import GLuaFixer.Effects.Cli (runCliIO)
import GLuaFixer.Effects.Files (runFilesIO)
import GLuaFixer.Effects.Interruptible (runInterruptible)
import GLuaFixer.Effects.Logging (runLoggingIO)

main :: IO ()
main = do
  -- Set the encoding of the program to UTF-8. This is to prevent problems when the system's locale
  -- is set to anything but UTF-8.
  -- See https://github.com/FPtje/GLuaFixer/issues/145
  setLocaleEncoding utf8

  exitCode <-
    runEff $
      runCliIO $
      runFilesIO $
      runInterruptible $
      Env.runEnvironment $
      runLoggingIO
      run

  exitWith exitCode
