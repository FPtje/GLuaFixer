{-# LANGUAGE OverloadedStrings #-}
module GLuaFixer.LintMessage where

import GLua.AG.Token
import GLua.AG.PrettyPrint
import Data.Aeson
import Control.Monad
import Data.Char
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

-- | Represents lint messages
data LintMessage
  = LintError { err_rg :: Region, err_msg :: String, err_file :: String }
  | LintWarning { warn_rg :: Region, warn_msg :: String, warn_file :: String }
  deriving ( Eq )

instance Show LintMessage where
    show lintMsg = formatLintMessageDefault lintMsg

formatLintMessage :: LintMessage -> LogFormat -> String
formatLintMessage lintMsg format = do
  case format of
    GithubLogFormat -> formatLintMessageGithub lintMsg
    _ -> formatLintMessageDefault lintMsg

formatLintMessageDefault :: LintMessage -> String
formatLintMessageDefault lintMsg = do
  let (rg, msg, file, level) = case lintMsg of
        LintError _rg _msg _file -> (_rg, _msg, _file, "Error")
        LintWarning _rg _msg _file -> (_rg, _msg, _file, "Warning")
  file ++ ": [" ++ level ++ "] " ++ (renderRegion rg) ++ ": " ++ msg

formatLintMessageGithub :: LintMessage -> String
formatLintMessageGithub lintMsg = do
  let (rg, msg, file, level) = case lintMsg of
        LintError _rg _msg _file -> (_rg, _msg, _file, "Error")
        LintWarning _rg _msg _file -> (_rg, _msg, _file, "Warning")
  let (Region start _) = rg
  let (LineColPos line col _) = start
  "::" ++ map toLower level ++ " file=" ++ file ++ ",line=" ++ show (succ line) ++ ",col=" ++ show (succ col) ++ "::" ++ msg

-- | Sort lint messages on file and then region
sortLintMessages :: [LintMessage] -> [LintMessage]
sortLintMessages msgs =
  let
    mapper :: LintMessage -> (String, Region)
    mapper (LintError rg _ f) = (f, rg)
    mapper (LintWarning rg _ f) = (f, rg)
  in
    sortOn mapper msgs
