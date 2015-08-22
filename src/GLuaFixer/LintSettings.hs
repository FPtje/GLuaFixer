{-# LANGUAGE OverloadedStrings #-}
module GLuaFixer.LintSettings where

import Data.Aeson
import Control.Monad

data LintSettings = LintSettings {
                        maxScopeDepth :: Int,
                        reportSyntaxErrors :: Bool,
                        reportSyntaxInconsistencies :: Bool,
                        reportDeprecated :: Bool,
                        reportWhitespaceStyle :: Bool,
                        reportBeginnerMistakes :: Bool,
                        reportEmptyBlocks :: Bool,
                        reportShadowing :: Bool,
                        reportGotos :: Bool,
                        reportDoubleNegations :: Bool,
                        reportDuplicateTableKeys :: Bool
                    } deriving (Show)

defaultLintSettings :: LintSettings
defaultLintSettings =   LintSettings {
                            maxScopeDepth = 7,
                            reportSyntaxErrors = False,
                            reportSyntaxInconsistencies = True,
                            reportDeprecated = True,
                            reportWhitespaceStyle = True,
                            reportBeginnerMistakes = True,
                            reportEmptyBlocks = True,
                            reportShadowing = True,
                            reportGotos = True,
                            reportDoubleNegations = True,
                            reportDuplicateTableKeys = True
                        }

instance FromJSON LintSettings where
    parseJSON (Object v) = LintSettings <$>
                               v .:? "maxScopeDepth" .!= (maxScopeDepth defaultLintSettings) <*>
                               v .:? "reportSyntaxErrors" .!= (reportSyntaxErrors defaultLintSettings) <*>
                               v .:? "reportSyntaxInconsistencies" .!= (reportSyntaxInconsistencies defaultLintSettings) <*>
                               v .:? "reportDeprecated" .!= (reportDeprecated defaultLintSettings) <*>
                               v .:? "reportWhitespaceStyle" .!= (reportWhitespaceStyle defaultLintSettings) <*>
                               v .:? "reportBeginnerMistakes" .!= (reportBeginnerMistakes defaultLintSettings) <*>
                               v .:? "reportEmptyBlocks" .!= (reportEmptyBlocks defaultLintSettings) <*>
                               v .:? "reportShadowing" .!= (reportShadowing defaultLintSettings) <*>
                               v .:? "reportGotos" .!= (reportGotos defaultLintSettings) <*>
                               v .:? "reportDoubleNegations" .!= (reportDoubleNegations defaultLintSettings) <*>
                               v .:? "reportDuplicateTableKeys" .!= (reportDuplicateTableKeys defaultLintSettings)

    parseJSON _          = mzero

instance ToJSON LintSettings where
    toJSON ls = object [
                        "maxScopeDepth" .= maxScopeDepth ls,
                        "reportSyntaxErrors" .= reportSyntaxErrors ls,
                        "reportSyntaxInconsistencies" .= reportSyntaxInconsistencies ls,
                        "reportDeprecated" .= reportDeprecated ls,
                        "reportWhitespaceStyle" .= reportWhitespaceStyle ls,
                        "reportBeginnerMistakes" .= reportBeginnerMistakes ls,
                        "reportEmptyBlocks" .= reportBeginnerMistakes ls,
                        "reportShadowing" .= reportBeginnerMistakes ls,
                        "reportGotos" .= reportBeginnerMistakes ls,
                        "reportDoubleNegations" .= reportBeginnerMistakes ls,
                        "reportDuplicateTableKeys" .= reportBeginnerMistakes ls
                       ]
