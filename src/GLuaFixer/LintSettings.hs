{-# LANGUAGE OverloadedStrings #-}
module GLuaFixer.LintSettings where

import Data.Aeson
import Control.Monad

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
                        lint_profanity :: Bool
                    } deriving (Show)

defaultLintSettings :: LintSettings
defaultLintSettings =   LintSettings {
                            lint_maxScopeDepth = 7,
                            lint_syntaxErrors = False,
                            lint_syntaxInconsistencies = True,
                            lint_deprecated = True,
                            lint_whitespaceStyle = True,
                            lint_beginnerMistakes = True,
                            lint_emptyBlocks = True,
                            lint_shadowing = True,
                            lint_gotos = True,
                            lint_doubleNegations = True,
                            lint_duplicateTableKeys = True,
                            lint_profanity = True
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
                               v .:? "lint_profanity" .!= (lint_profanity defaultLintSettings)

    parseJSON _          = mzero

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
                        "lint_profanity" .= lint_profanity ls
                       ]
