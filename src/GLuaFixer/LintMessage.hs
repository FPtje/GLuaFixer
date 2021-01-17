{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module GLuaFixer.LintMessage where

import Control.Monad
import Data.Aeson
import Data.List (sortOn)
import Data.Maybe (isJust)
import System.Environment (lookupEnv)
import Text.ParserCombinators.UU.BasicInstances hiding (msgs)

import GLua.AG.Token
import GLua.AG.PrettyPrint

-- | Output formats for logging
data LogFormat = StandardLogFormat | GithubLogFormat
data LogFormatChoice = AutoLogFormatChoice | LogFormatChoice !LogFormat

instance Show LogFormat where
  show StandardLogFormat = "standard"
  show GithubLogFormat = "github"

instance Show LogFormatChoice where
  show (LogFormatChoice choice) = show choice
  show AutoLogFormatChoice = "auto"

instance ToJSON LogFormat where
  toJSON StandardLogFormat = "standard"
  toJSON GithubLogFormat = "github"

instance ToJSON LogFormatChoice where
  toJSON (LogFormatChoice choice) = toJSON choice
  toJSON AutoLogFormatChoice = "auto"

instance FromJSON LogFormatChoice where
  parseJSON (String logFormat) = case logFormat of
    "standard" -> pure $ LogFormatChoice StandardLogFormat
    "github" -> pure $ LogFormatChoice GithubLogFormat
    "auto" -> pure AutoLogFormatChoice
    _ -> fail ( "Please use either \"auto\" \"standard\" or \"github\" but was " ++ show logFormat )
  parseJSON _ = mzero

data Severity = LintWarning | LintError
  deriving (Eq)

-- | Represents lint messages
data LintMessage
  = LintMessage
    { lintmsg_severity :: !Severity
    , lintmsg_region   :: !Region
    , lintmsg_message  :: !String
    , lintmsg_file     :: !FilePath
    }
  deriving (Eq)

instance Show LintMessage where
    show lintMsg = formatLintMessageDefault lintMsg

logFormatChoiceToLogFormat :: LogFormatChoice -> IO LogFormat
logFormatChoiceToLogFormat = \case
  LogFormatChoice format -> pure format
  AutoLogFormatChoice -> do
    actionsExists <- isJust <$> lookupEnv "GITHUB_ACTIONS"
    workflowExists <- isJust <$> lookupEnv "GITHUB_WORKFLOW"
    if actionsExists && workflowExists
    then pure GithubLogFormat
    else pure StandardLogFormat


formatLintMessage :: LogFormat -> LintMessage -> String
formatLintMessage StandardLogFormat lintMsg = formatLintMessageDefault lintMsg
formatLintMessage GithubLogFormat lintMsg = formatLintMessageGithub lintMsg

formatLintMessageDefault :: LintMessage -> String
formatLintMessageDefault (LintMessage severity region msg file) =
  let
    level = case severity of
      LintWarning -> "Warning"
      LintError -> "Error"
  in
    showString file .
    showString ": [" . showString level . showString "] " .
    showString (renderRegion region) . showString ": " .
    showString msg $
    ""

formatLintMessageGithub :: LintMessage -> String
formatLintMessageGithub (LintMessage severity (Region (LineColPos line col _) _) msg file) =
  let
    level = case severity of
      LintWarning -> "warning"
      LintError -> "error"
  in
    showString "::" . showString level .
    showString " file=" . showString file .
    showString ",line=" . shows (succ line) .
    showString ",col=" . shows (succ col) .
    showString "::" . showString msg $
    ""

-- | Sort lint messages on file and then region
sortLintMessages :: [LintMessage] -> [LintMessage]
sortLintMessages msgs =
    sortOn (\(LintMessage _ rg _ f) -> (f, rg)) msgs
