{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GLuaFixer.Effects.Logging where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import qualified Effectful.Environment as Env
import Effectful.TH (makeEffect)
import GLuaFixer.LintMessage (LintMessage, LogFormat (..), LogFormatChoice (..), formatLintMessage)
import System.IO (hPutStrLn, stderr)

-- | The effect for emitting lint messages to the right channel
data Logging :: Effect where
  -- | Emit a single lint message in the given format
  EmitLintMessage :: LogFormat -> LintMessage -> Logging m ()
  -- | Find out which logging format to use
  GetLogFormat :: LogFormatChoice -> Logging m LogFormat
  -- | Print a string in stdout
  PutStrLnStdOut :: String -> Logging m ()
  -- | Print a string in stdout
  PutStrStdOut :: String -> Logging m ()
  -- | Print a string in stderr
  PutStrLnStdError :: String -> Logging m ()

type instance DispatchOf Logging = Dynamic

makeEffect ''Logging

-- | Run the logging in IO
runLoggingIO :: (IOE :> es, Env.Environment :> es) => Eff (Logging : es) a -> Eff es a
runLoggingIO = interpret $ \_ -> \case
  EmitLintMessage logFormat lintMessage -> case logFormat of
    StandardLogFormat ->
      liftIO $ putStrLn $ formatLintMessage StandardLogFormat lintMessage
    GithubLogFormat -> do
      liftIO $ putStrLn $ formatLintMessage GithubLogFormat lintMessage
      liftIO $ putStrLn $ formatLintMessage StandardLogFormat lintMessage
  GetLogFormat (LogFormatChoice format) -> pure format
  GetLogFormat AutoLogFormatChoice -> do
    actionsExists <- isJust <$> Env.lookupEnv "GITHUB_ACTIONS"
    workflowExists <- isJust <$> Env.lookupEnv "GITHUB_WORKFLOW"

    pure $
      if actionsExists && workflowExists
        then GithubLogFormat
        else StandardLogFormat
  PutStrLnStdOut str ->
    liftIO $ putStrLn str
  PutStrStdOut str ->
    liftIO $ putStr str
  PutStrLnStdError str ->
    liftIO $ hPutStrLn stderr str

runLoggingPureNoop :: Eff (Logging : es) a -> Eff es a
runLoggingPureNoop = interpret $ \_ -> \case
  EmitLintMessage _ _ -> pure ()
  GetLogFormat (LogFormatChoice format) -> pure format
  GetLogFormat AutoLogFormatChoice -> pure StandardLogFormat
  PutStrLnStdOut _ -> pure ()
  PutStrStdOut _ -> pure ()
  PutStrLnStdError _ -> pure ()
