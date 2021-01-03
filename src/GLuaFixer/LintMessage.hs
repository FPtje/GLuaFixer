module GLuaFixer.LintMessage where

import GLua.AG.Token
import GLua.AG.PrettyPrint
import Data.Char
import Data.List (sortOn)
import Text.ParserCombinators.UU.BasicInstances hiding (msgs)

-- | Represents lint messages
data LintMessage
  = LintError { err_rg :: Region, err_msg :: String, err_file :: String }
  | LintWarning { warn_rg :: Region, warn_msg :: String, warn_file :: String }
  deriving ( Eq )

instance Show LintMessage where
    show lintMsg = formatLintMessageDefault lintMsg

formatLintMessage :: LintMessage -> String -> String
formatLintMessage lintMsg format = do
  case format of
    "github" -> formatLintMessageGithub lintMsg
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
