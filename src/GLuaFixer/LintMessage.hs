{-# LANGUAGE OverloadedStrings #-}
module GLuaFixer.LintMessage where

import GLua.AG.Token
import GLua.AG.PrettyPrint
import Data.Aeson
import Control.Monad
import Data.List (sortOn)
import Text.ParserCombinators.UU.BasicInstances hiding (msgs)

data LogFormat = StandardLogFormat | GithubLogFormat

instance Show LogFormat where
  show (StandardLogFormat) = "standard"
  show (GithubLogFormat) = "github"

instance ToJSON LogFormat where
  toJSON StandardLogFormat = "standard"
  toJSON GithubLogFormat = "github"

instance FromJSON LogFormat where
  parseJSON (String logFormat) = case logFormat of
    "standard" -> return ( StandardLogFormat )
    "github" -> return ( GithubLogFormat )
    _ -> fail ( "Please use either \"standard\" or \"github\" but was " ++ show logFormat )
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
