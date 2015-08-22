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
                        reportBeginnerMistakes :: Bool
                    } deriving (Show)

defaultLintSettings :: LintSettings
defaultLintSettings =   LintSettings {
                            maxScopeDepth = 7,
                            reportSyntaxErrors = False,
                            reportSyntaxInconsistencies = True,
                            reportDeprecated = True,
                            reportWhitespaceStyle = True,
                            reportBeginnerMistakes = True
                        }

instance FromJSON LintSettings where
    parseJSON (Object v) = LintSettings <$>
                               v .:? "maxScopeDepth" .!= (maxScopeDepth defaultLintSettings) <*>
                               v .:? "reportSyntaxErrors" .!= (reportSyntaxErrors defaultLintSettings) <*>
                               v .:? "reportSyntaxInconsistencies" .!= (reportSyntaxInconsistencies defaultLintSettings) <*>
                               v .:? "reportDeprecated" .!= (reportDeprecated defaultLintSettings) <*>
                               v .:? "reportWhitespaceStyle" .!= (reportWhitespaceStyle defaultLintSettings) <*>
                               v .:? "reportBeginnerMistakes" .!= (reportBeginnerMistakes defaultLintSettings)

    parseJSON _          = mzero

instance ToJSON LintSettings where
    toJSON ls = object [
                        "maxScopeDepth" .= maxScopeDepth ls,
                        "reportSyntaxErrors" .= reportSyntaxErrors ls,
                        "reportSyntaxInconsistencies" .= reportSyntaxInconsistencies ls,
                        "reportDeprecated" .= reportDeprecated ls,
                        "reportWhitespaceStyle" .= reportWhitespaceStyle ls,
                        "reportBeginnerMistakes" .= reportBeginnerMistakes ls
                       ]
