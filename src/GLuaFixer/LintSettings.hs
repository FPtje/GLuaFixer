{-# LANGUAGE OverloadedStrings #-}
module GLuaFixer.LintSettings where

import Data.Aeson
import Control.Monad
import GLua.AG.PrettyPrint
import GLuaFixer.LintMessage

data LintSettings =
    LintSettings
    { lint_maxScopeDepth             :: !Int
    , lint_syntaxErrors              :: !Bool
    , lint_syntaxInconsistencies     :: !Bool
    , lint_deprecated                :: !Bool
    , lint_trailingWhitespace        :: !Bool
    , lint_whitespaceStyle           :: !Bool
    , lint_beginnerMistakes          :: !Bool
    , lint_emptyBlocks               :: !Bool
    , lint_shadowing                 :: !Bool
    , lint_gotos                     :: !Bool
    , lint_doubleNegations           :: !Bool
    , lint_redundantIfStatements     :: !Bool
    , lint_redundantParentheses      :: !Bool
    , lint_duplicateTableKeys        :: !Bool
    , lint_profanity                 :: !Bool
    , lint_unusedVars                :: !Bool
    , lint_unusedParameters          :: !Bool
    , lint_unusedLoopVars            :: !Bool
    , lint_ignoreFiles               :: ![String]

    , prettyprint_spaceAfterParens   :: !Bool
    , prettyprint_spaceAfterBrackets :: !Bool
    , prettyprint_spaceAfterBraces   :: !Bool
    , prettyprint_spaceAfterLabel    :: !Bool
    , prettyprint_spaceBeforeComma   :: !Bool
    , prettyprint_spaceAfterComma    :: !Bool
    , prettyprint_semicolons         :: !Bool
    , prettyprint_cStyle             :: !Bool
    , prettyprint_rejectInvalidCode  :: !Bool
    , prettyprint_indentation        :: !String

    , log_format                     :: !LogFormat
    } deriving (Show)

defaultLintSettings :: LintSettings
defaultLintSettings =
    LintSettings
    { lint_maxScopeDepth             = 7
    , lint_syntaxErrors              = True
    , lint_syntaxInconsistencies     = True
    , lint_deprecated                = True
    , lint_trailingWhitespace        = True
    , lint_whitespaceStyle           = True
    , lint_beginnerMistakes          = True
    , lint_emptyBlocks               = True
    , lint_shadowing                 = True
    , lint_gotos                     = True
    , lint_doubleNegations           = True
    , lint_redundantIfStatements     = True
    , lint_redundantParentheses      = True
    , lint_duplicateTableKeys        = True
    , lint_profanity                 = True
    , lint_unusedVars                = True
    , lint_unusedParameters          = False
    , lint_unusedLoopVars            = False
    , lint_ignoreFiles               = []

    , prettyprint_spaceAfterParens   = False
    , prettyprint_spaceAfterBrackets = False
    , prettyprint_spaceAfterBraces   = False
    , prettyprint_spaceAfterLabel    = False
    , prettyprint_spaceBeforeComma   = False
    , prettyprint_spaceAfterComma    = True
    , prettyprint_semicolons         = False
    , prettyprint_cStyle             = False
    , prettyprint_rejectInvalidCode  = False
    , prettyprint_indentation        = "    "

    , log_format                     = StandardLogFormat
  }

instance FromJSON LintSettings where
    parseJSON (Object v) =
        LintSettings <$>
          v .:? "lint_maxScopeDepth"             .!= lint_maxScopeDepth defaultLintSettings             <*>
          v .:? "lint_syntaxErrors"              .!= lint_syntaxErrors defaultLintSettings              <*>
          v .:? "lint_syntaxInconsistencies"     .!= lint_syntaxInconsistencies defaultLintSettings     <*>
          v .:? "lint_deprecated"                .!= lint_deprecated defaultLintSettings                <*>
          v .:? "lint_trailingWhitespace"        .!= lint_trailingWhitespace defaultLintSettings        <*>
          v .:? "lint_whitespaceStyle"           .!= lint_whitespaceStyle defaultLintSettings           <*>
          v .:? "lint_beginnerMistakes"          .!= lint_beginnerMistakes defaultLintSettings          <*>
          v .:? "lint_emptyBlocks"               .!= lint_emptyBlocks defaultLintSettings               <*>
          v .:? "lint_shadowing"                 .!= lint_shadowing defaultLintSettings                 <*>
          v .:? "lint_gotos"                     .!= lint_gotos defaultLintSettings                     <*>
          v .:? "lint_doubleNegations"           .!= lint_doubleNegations defaultLintSettings           <*>
          v .:? "lint_redundantIfStatements"     .!= lint_redundantIfStatements defaultLintSettings     <*>
          v .:? "lint_redundantParentheses"      .!= lint_redundantParentheses defaultLintSettings      <*>
          v .:? "lint_duplicateTableKeys"        .!= lint_duplicateTableKeys defaultLintSettings        <*>
          v .:? "lint_profanity"                 .!= lint_profanity defaultLintSettings                 <*>
          v .:? "lint_unusedVars"                .!= lint_unusedVars defaultLintSettings                <*>
          v .:? "lint_unusedParameters"          .!= lint_unusedParameters defaultLintSettings          <*>
          v .:? "lint_unusedLoopVars"            .!= lint_unusedLoopVars defaultLintSettings            <*>
          v .:? "lint_ignoreFiles"               .!= lint_ignoreFiles defaultLintSettings               <*>
          v .:? "prettyprint_spaceAfterParens"   .!= prettyprint_spaceAfterParens defaultLintSettings   <*>
          v .:? "prettyprint_spaceAfterBrackets" .!= prettyprint_spaceAfterBrackets defaultLintSettings <*>
          v .:? "prettyprint_spaceAfterBraces"   .!= prettyprint_spaceAfterBraces defaultLintSettings   <*>
          v .:? "prettyprint_spaceAfterLabel"    .!= prettyprint_spaceAfterLabel defaultLintSettings    <*>
          v .:? "prettyprint_spaceBeforeComma"   .!= prettyprint_spaceBeforeComma defaultLintSettings   <*>
          v .:? "prettyprint_spaceAfterComma"    .!= prettyprint_spaceAfterComma defaultLintSettings    <*>
          v .:? "prettyprint_semicolons"         .!= prettyprint_semicolons defaultLintSettings         <*>
          v .:? "prettyprint_cStyle"             .!= prettyprint_cStyle defaultLintSettings             <*>
          v .:? "prettyprint_rejectInvalidCode"  .!= prettyprint_rejectInvalidCode defaultLintSettings  <*>
          v .:? "prettyprint_indentation"        .!= prettyprint_indentation defaultLintSettings        <*>
          v .:? "log_format"                     .!= log_format defaultLintSettings

    parseJSON _          = mzero

lint2ppSetting :: LintSettings -> PrettyPrintConfig
lint2ppSetting ls =
    PPConfig
    { spaceAfterParens   = prettyprint_spaceAfterParens ls
    , spaceAfterBrackets = prettyprint_spaceAfterBrackets ls
    , spaceAfterBraces   = prettyprint_spaceAfterBraces ls
    , spaceAfterLabel    = prettyprint_spaceAfterLabel ls
    , spaceBeforeComma   = prettyprint_spaceBeforeComma ls
    , spaceAfterComma    = prettyprint_spaceAfterComma ls
    , semicolons         = prettyprint_semicolons ls
    , cStyle             = prettyprint_cStyle ls
    , indentation        = prettyprint_indentation ls
    }

instance ToJSON LintSettings where
    toJSON ls = object
        [ "lint_maxScopeDepth"             .= lint_maxScopeDepth ls
        , "lint_syntaxErrors"              .= lint_syntaxErrors ls
        , "lint_syntaxInconsistencies"     .= lint_syntaxInconsistencies ls
        , "lint_deprecated"                .= lint_deprecated ls
        , "lint_trailingWhitespace"        .= lint_trailingWhitespace ls
        , "lint_whitespaceStyle"           .= lint_whitespaceStyle ls
        , "lint_beginnerMistakes"          .= lint_beginnerMistakes ls
        , "lint_emptyBlocks"               .= lint_emptyBlocks ls
        , "lint_shadowing"                 .= lint_shadowing ls
        , "lint_gotos"                     .= lint_gotos ls
        , "lint_doubleNegations"           .= lint_doubleNegations ls
        , "lint_redundantIfStatements"     .= lint_redundantIfStatements ls
        , "lint_redundantParentheses"      .= lint_redundantParentheses ls
        , "lint_duplicateTableKeys"        .= lint_duplicateTableKeys ls
        , "lint_profanity"                 .= lint_profanity ls
        , "lint_unusedVars"                .= lint_unusedVars ls
        , "lint_unusedParameters"          .= lint_unusedParameters ls
        , "lint_unusedLoopVars"            .= lint_unusedLoopVars ls
        , "prettyprint_spaceAfterParens"   .= prettyprint_spaceAfterParens ls
        , "prettyprint_spaceAfterBrackets" .= prettyprint_spaceAfterBrackets ls
        , "prettyprint_spaceAfterBraces"   .= prettyprint_spaceAfterBraces ls
        , "prettyprint_spaceAfterLabel"    .= prettyprint_spaceAfterLabel ls
        , "prettyprint_spaceBeforeComma"   .= prettyprint_spaceBeforeComma ls
        , "prettyprint_spaceAfterComma"    .= prettyprint_spaceAfterComma ls
        , "prettyprint_semicolons"         .= prettyprint_semicolons ls
        , "prettyprint_cStyle"             .= prettyprint_cStyle ls
        , "prettyprint_indentation"        .= prettyprint_indentation ls
        , "log_format"                     .= log_format ls
        ]
