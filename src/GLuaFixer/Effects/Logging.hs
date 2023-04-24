{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
module GLuaFixer.Effects.Logging where


import Effectful ( Effect, Dispatch(Dynamic), DispatchOf, IOE, (:>), Eff )
import Effectful.TH ( makeEffect )
import GLuaFixer.LintMessage (LintMessage, LogFormat(..), LogFormatChoice (..), formatLintMessage)
import qualified Effectful.Environment as Env
import Effectful.Dispatch.Dynamic (interpret)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)

-- | The effect for emitting lint messages to the right channel
data Logging :: Effect where
  -- | Emit a single lint message in the given format
  EmitLintMessage :: LogFormat -> LintMessage -> Logging m ()

  -- | Find out which logging format to use
  GetLogFormat :: LogFormatChoice -> Logging m LogFormat

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

    pure $ if actionsExists && workflowExists
      then GithubLogFormat
      else StandardLogFormat

runLoggingPureNoop :: Eff (Logging : es) a -> Eff es a
runLoggingPureNoop = interpret $ \_ -> \case
  EmitLintMessage _ _ -> pure ()
  GetLogFormat (LogFormatChoice format) -> pure format
  GetLogFormat AutoLogFormatChoice -> pure StandardLogFormat
