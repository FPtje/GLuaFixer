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
    show (LintError rg msg file) = formatLintMessageDefault "Error" rg msg file
    show (LintWarning rg msg file) = formatLintMessageDefault "Warning" rg msg file

getLintMessageData :: LintMessage -> (Region, String, String, String)
getLintMessageData (LintError rg msg file) = (rg, msg, file, "Error")
getLintMessageData (LintWarning rg msg file) = (rg, msg, file, "Warning")

formatLintMessage :: LintMessage -> String -> String
formatLintMessage lintMsg format = do
  let (rg, msg, file, level) = getLintMessageData lintMsg
  case format of
    "github" -> formatLintMessageGithub level rg msg file
    _ -> formatLintMessageDefault level rg msg file

formatLintMessageDefault :: String -> Region -> String -> String -> String
formatLintMessageDefault level rg msg file = file ++ ": [" ++ level ++ "] " ++ (renderRegion rg) ++ ": " ++ msg

formatLintMessageGithub :: String -> Region -> String -> String -> String
formatLintMessageGithub level rg msg file = do
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
