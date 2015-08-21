{-# LANGUAGE OverloadedStrings #-}
module GLuaFixer.LintSettings where

import Data.Aeson
import Control.Monad

data LintSettings = LintSettings {
                        maxScopeDepth :: Int
                    } deriving (Show)

defaultLintSettings :: LintSettings
defaultLintSettings =   LintSettings {
                            maxScopeDepth = 7
                        }

instance FromJSON LintSettings where
    parseJSON (Object v) = LintSettings <$>
                               v .:? "maxScopeDepth" .!= (maxScopeDepth defaultLintSettings)

    parseJSON _          = mzero

instance ToJSON LintSettings where
    toJSON ls = object [
                        "maxScopeDepth" .= maxScopeDepth ls
                       ]
