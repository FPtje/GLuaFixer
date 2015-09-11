{-# LANGUAGE OverloadedStrings #-}
module GLuaFixer.LintSettings where

import Data.Aeson
import Control.Monad
import GLua.AG.PrettyPrint

data LintSettings = LintSettings {
                        lint_maxScopeDepth :: Int,
                        lint_syntaxErrors :: Bool,
                        lint_syntaxInconsistencies :: Bool,
                        lint_deprecated :: Bool,
                        lint_whitespaceStyle :: Bool,
                        lint_beginnerMistakes :: Bool,
                        lint_emptyBlocks :: Bool,
                        lint_shadowing :: Bool,
                        lint_gotos :: Bool,
                        lint_doubleNegations :: Bool,
                        lint_duplicateTableKeys :: Bool,
                        lint_profanity :: Bool,
                        lint_unusedVars :: Bool,
                        lint_unusedParameters :: Bool,
                        lint_unusedLoopVars :: Bool,

                        prettyprint_spaceAfterParens :: Bool,
                        prettyprint_spaceAfterBrackets :: Bool,
                        prettyprint_spaceAfterBraces :: Bool,
                        prettyprint_semicolons :: Bool,
                        prettyprint_cStyle :: Bool,
                        prettyprint_indentation :: String
                    } deriving (Show)

defaultLintSettings :: LintSettings
defaultLintSettings =   LintSettings {
                            lint_maxScopeDepth = 7,
                            lint_syntaxErrors = True,
                            lint_syntaxInconsistencies = True,
                            lint_deprecated = True,
                            lint_whitespaceStyle = True,
                            lint_beginnerMistakes = True,
                            lint_emptyBlocks = True,
                            lint_shadowing = True,
                            lint_gotos = True,
                            lint_doubleNegations = True,
                            lint_duplicateTableKeys = True,
                            lint_profanity = True,
                            lint_unusedVars = True,
                            lint_unusedParameters = False,
                            lint_unusedLoopVars = False,

                            prettyprint_spaceAfterParens = False,
                            prettyprint_spaceAfterBrackets = False,
                            prettyprint_spaceAfterBraces = False,
                            prettyprint_semicolons = False,
                            prettyprint_cStyle = False,
                            prettyprint_indentation = "    "
                        }

instance FromJSON LintSettings where
    parseJSON (Object v) = LintSettings <$>
                               v .:? "lint_maxScopeDepth" .!= (lint_maxScopeDepth defaultLintSettings) <*>
                               v .:? "lint_syntaxErrors" .!= (lint_syntaxErrors defaultLintSettings) <*>
                               v .:? "lint_syntaxInconsistencies" .!= (lint_syntaxInconsistencies defaultLintSettings) <*>
                               v .:? "lint_deprecated" .!= (lint_deprecated defaultLintSettings) <*>
                               v .:? "lint_whitespaceStyle" .!= (lint_whitespaceStyle defaultLintSettings) <*>
                               v .:? "lint_beginnerMistakes" .!= (lint_beginnerMistakes defaultLintSettings) <*>
                               v .:? "lint_emptyBlocks" .!= (lint_emptyBlocks defaultLintSettings) <*>
                               v .:? "lint_shadowing" .!= (lint_shadowing defaultLintSettings) <*>
                               v .:? "lint_gotos" .!= (lint_gotos defaultLintSettings) <*>
                               v .:? "lint_doubleNegations" .!= (lint_doubleNegations defaultLintSettings) <*>
                               v .:? "lint_duplicateTableKeys" .!= (lint_duplicateTableKeys defaultLintSettings) <*>
                               v .:? "lint_profanity" .!= (lint_profanity defaultLintSettings) <*>
                               v .:? "lint_unusedVars" .!= (lint_unusedVars defaultLintSettings) <*>
                               v .:? "lint_unusedParameters" .!= (lint_unusedParameters defaultLintSettings) <*>
                               v .:? "lint_unusedLoopVars" .!= (lint_unusedLoopVars defaultLintSettings) <*>
                               v .:? "prettyprint_spaceAfterParens" .!= (prettyprint_spaceAfterParens defaultLintSettings) <*>
                               v .:? "prettyprint_spaceAfterBrackets" .!= (prettyprint_spaceAfterBrackets defaultLintSettings) <*>
                               v .:? "prettyprint_spaceAfterBraces" .!= (prettyprint_spaceAfterBraces defaultLintSettings) <*>
                               v .:? "prettyprint_semicolons" .!= (prettyprint_semicolons defaultLintSettings) <*>
                               v .:? "prettyprint_cStyle" .!= (prettyprint_cStyle defaultLintSettings) <*>
                               v .:? "prettyprint_indentation" .!= (prettyprint_indentation defaultLintSettings)

    parseJSON _          = mzero

lint2ppSetting :: LintSettings -> PrettyPrintConfig
lint2ppSetting ls = PPConfig{
  spaceAfterParens = prettyprint_spaceAfterParens ls,
  spaceAfterBrackets = prettyprint_spaceAfterBrackets ls,
  spaceAfterBraces = prettyprint_spaceAfterBraces ls,
  semicolons = prettyprint_semicolons ls,
  cStyle = prettyprint_cStyle ls,
  indentation = prettyprint_indentation ls
}

instance ToJSON LintSettings where
    toJSON ls = object [
                        "lint_maxScopeDepth" .= lint_maxScopeDepth ls,
                        "lint_syntaxErrors" .= lint_syntaxErrors ls,
                        "lint_syntaxInconsistencies" .= lint_syntaxInconsistencies ls,
                        "lint_deprecated" .= lint_deprecated ls,
                        "lint_whitespaceStyle" .= lint_whitespaceStyle ls,
                        "lint_beginnerMistakes" .= lint_beginnerMistakes ls,
                        "lint_emptyBlocks" .= lint_emptyBlocks ls,
                        "lint_shadowing" .= lint_shadowing ls,
                        "lint_gotos" .= lint_gotos ls,
                        "lint_doubleNegations" .= lint_doubleNegations ls,
                        "lint_duplicateTableKeys" .= lint_duplicateTableKeys ls,
                        "lint_profanity" .= lint_profanity ls,
                        "lint_unusedVars" .= lint_unusedVars ls,
                        "lint_unusedParameters" .= lint_unusedParameters ls,
                        "lint_unusedLoopVars" .= lint_unusedLoopVars ls,
                        "prettyprint_spaceAfterParens" .= prettyprint_spaceAfterParens ls,
                        "prettyprint_spaceAfterBrackets" .= prettyprint_spaceAfterBrackets ls,
                        "prettyprint_spaceAfterBraces" .= prettyprint_spaceAfterBraces ls,
                        "prettyprint_semicolons" .= prettyprint_semicolons ls,
                        "prettyprint_cStyle" .= prettyprint_cStyle ls,
                        "prettyprint_indentation" .= prettyprint_indentation ls
                       ]
