module GLuaFixer.LintMessage where

import GLua.AG.Token
import GLua.AG.PrettyPrint
import Data.List (sortOn)

-- | Represents lint messages
data LintMessage
  = LintError { err_rg :: Region, err_msg :: String, err_file :: String }
  | LintWarning {warn_rg :: Region, warn_msg :: String, warn_file :: String }
  deriving ( Eq )

instance Show LintMessage where
    show (LintError rg msg file) = showString file . showString ": [Error] " . showString (renderRegion rg) . showString ": " . showString msg $ ""
    show (LintWarning rg msg file) = showString file . showString ": [Warning] " . showString (renderRegion rg) . showString ": " . showString msg $ ""

-- | Sort lint messages on file and then region
sortLintMessages :: [LintMessage] -> [LintMessage]
sortLintMessages msgs =
  let
    mapper :: LintMessage -> (String, Region)
    mapper (LintError rg _ f) = (f, rg)
    mapper (LintWarning rg _ f) = (f, rg)
  in
    sortOn mapper msgs
