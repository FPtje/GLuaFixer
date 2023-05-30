{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module GLuaFixer.LintSettings where

import Control.Applicative ((<|>))
import Data.Aeson
    ( FromJSON(parseJSON),
      ToJSON(toJSON),
      (.!=),
      (.:?),
      object,
      Value(Object),
      KeyValue((.=)) )
import Control.Monad ( MonadPlus(mzero) )
import Data.String (IsString)
import GLua.AG.PrettyPrint ( PrettyPrintConfig(..) )
import GLuaFixer.LintMessage
    ( LogFormatChoice(AutoLogFormatChoice) )

-- | Indentation used for pretty printing code
newtype Indentation = Indentation { unIndentation :: String }
  deriving IsString
  deriving newtype Show

-- | Whether a file is read from stdin or from files
data StdInOrFiles
  = UseStdIn
  | UseFiles [FilePath]
  deriving (Show)

-- | Convert a string to StdInOrFiles
parseStdInOrFiles :: String -> StdInOrFiles
parseStdInOrFiles "stdin" = UseStdIn
parseStdInOrFiles other = UseFiles [other]

data LintSettings =
    LintSettings
    { lint_maxScopeDepth :: !Int
    , lint_syntaxErrors :: !Bool
    , lint_syntaxInconsistencies :: !Bool
    , lint_deprecated :: !Bool
    , lint_trailingWhitespace :: !Bool
    , lint_whitespaceStyle :: !Bool
    , lint_beginnerMistakes :: !Bool
    , lint_emptyBlocks :: !Bool
    , lint_shadowing :: !Bool
    , lint_gotos :: !Bool
    , lint_goto_identifier :: !Bool
    , lint_doubleNegations :: !Bool
    , lint_redundantIfStatements :: !Bool
    , lint_redundantParentheses :: !Bool
    , lint_duplicateTableKeys :: !Bool
    , lint_profanity :: !Bool
    , lint_unusedVars :: !Bool
    , lint_unusedParameters :: !Bool
    , lint_unusedLoopVars :: !Bool
    , lint_inconsistentVariableStyle :: !Bool
    , lint_spaceBetweenParens :: !Bool
    , lint_spaceBetweenBrackets :: !Bool
    , lint_spaceBetweenBraces :: !Bool
    , lint_spaceBeforeComma :: !Bool
    , lint_spaceAfterComma :: !Bool
    , lint_maxLineLength :: !Int
    , lint_ignoreFiles :: ![String]

    , prettyprint_spaceBetweenParens :: !Bool
    , prettyprint_spaceBetweenBrackets :: !Bool
    , prettyprint_spaceBetweenBraces :: !Bool
    , prettyprint_spaceEmptyParens :: !Bool
    , prettyprint_spaceEmptyBraces :: !Bool
    , prettyprint_spaceAfterLabel :: !Bool
    , prettyprint_spaceBeforeComma :: !Bool
    , prettyprint_spaceAfterComma :: !Bool
    , prettyprint_semicolons :: !Bool
    , prettyprint_cStyle :: !Bool
    , prettyprint_removeRedundantParens :: !Bool
    , prettyprint_minimizeParens :: !Bool
    , prettyprint_assumeOperatorAssociativity :: !Bool
    , prettyprint_rejectInvalidCode :: !Bool
    , prettyprint_indentation :: !String

    , log_format :: !LogFormatChoice
    } deriving (Show)

defaultLintSettings :: LintSettings
defaultLintSettings =
    LintSettings
    { lint_maxScopeDepth = 7
    , lint_syntaxErrors = True
    , lint_syntaxInconsistencies = True
    , lint_deprecated = True
    , lint_trailingWhitespace = True
    , lint_whitespaceStyle = True
    , lint_beginnerMistakes = True
    , lint_emptyBlocks = True
    , lint_shadowing = True
    , lint_gotos = True
    , lint_goto_identifier = True
    , lint_doubleNegations = True
    , lint_redundantIfStatements = True
    , lint_redundantParentheses = True
    , lint_duplicateTableKeys = True
    , lint_profanity = True
    , lint_unusedVars = True
    , lint_unusedParameters = False
    , lint_unusedLoopVars = False
    , lint_inconsistentVariableStyle = False
    , lint_spaceBetweenParens = False
    , lint_spaceBetweenBrackets = False
    , lint_spaceBetweenBraces = False
    , lint_spaceBeforeComma = False
    , lint_spaceAfterComma = False
    , lint_maxLineLength = 0
    , lint_ignoreFiles = []

    , prettyprint_spaceBetweenParens = False
    , prettyprint_spaceBetweenBrackets = False
    , prettyprint_spaceBetweenBraces = False
    , prettyprint_spaceEmptyParens = True
    , prettyprint_spaceEmptyBraces = True
    , prettyprint_spaceAfterLabel = False
    , prettyprint_spaceBeforeComma = False
    , prettyprint_spaceAfterComma = True
    , prettyprint_semicolons = False
    , prettyprint_cStyle = False
    , prettyprint_removeRedundantParens = True
    , prettyprint_minimizeParens = False
    , prettyprint_assumeOperatorAssociativity = True
    , prettyprint_rejectInvalidCode = False
    , prettyprint_indentation = "    "

    , log_format = AutoLogFormatChoice
  }

instance FromJSON LintSettings where
    parseJSON (Object v) =
        LintSettings <$>
          v .:? "lint_maxScopeDepth" .!= lint_maxScopeDepth defaultLintSettings <*>
          v .:? "lint_syntaxErrors" .!= lint_syntaxErrors defaultLintSettings <*>
          v .:? "lint_syntaxInconsistencies" .!= lint_syntaxInconsistencies defaultLintSettings <*>
          v .:? "lint_deprecated" .!= lint_deprecated defaultLintSettings <*>
          v .:? "lint_trailingWhitespace" .!= lint_trailingWhitespace defaultLintSettings <*>
          v .:? "lint_whitespaceStyle" .!= lint_whitespaceStyle defaultLintSettings <*>
          v .:? "lint_beginnerMistakes" .!= lint_beginnerMistakes defaultLintSettings <*>
          v .:? "lint_emptyBlocks" .!= lint_emptyBlocks defaultLintSettings <*>
          v .:? "lint_shadowing" .!= lint_shadowing defaultLintSettings <*>
          v .:? "lint_gotos" .!= lint_gotos defaultLintSettings <*>
          v .:? "lint_goto_identifier" .!= lint_goto_identifier defaultLintSettings <*>
          v .:? "lint_doubleNegations" .!= lint_doubleNegations defaultLintSettings <*>
          v .:? "lint_redundantIfStatements" .!= lint_redundantIfStatements defaultLintSettings <*>
          v .:? "lint_redundantParentheses" .!= lint_redundantParentheses defaultLintSettings <*>
          v .:? "lint_duplicateTableKeys" .!= lint_duplicateTableKeys defaultLintSettings <*>
          v .:? "lint_profanity" .!= lint_profanity defaultLintSettings <*>
          v .:? "lint_unusedVars" .!= lint_unusedVars defaultLintSettings <*>
          v .:? "lint_unusedParameters" .!= lint_unusedParameters defaultLintSettings <*>
          v .:? "lint_unusedLoopVars" .!= lint_unusedLoopVars defaultLintSettings <*>
          v .:? "lint_inconsistentVariableStyle" .!= lint_inconsistentVariableStyle defaultLintSettings <*>

          -- Backwards compatible change: accept both the newer spaceBetween and the older
          -- spaceAfter
          (v .:? "lint_spaceBetweenParens" .!= lint_spaceBetweenParens defaultLintSettings <|>
          v .:? "lint_spaceAfterParens" .!= lint_spaceBetweenParens defaultLintSettings) <*>

          (v .:? "lint_spaceBetweenBrackets" .!= lint_spaceBetweenBrackets defaultLintSettings <|>
          v .:? "lint_spaceAfterBrackets" .!= lint_spaceBetweenBrackets defaultLintSettings) <*>

          (v .:? "lint_spaceBetweenBraces" .!= lint_spaceBetweenBraces defaultLintSettings <|>
          v .:? "lint_spaceAfterBraces" .!= lint_spaceBetweenBraces defaultLintSettings) <*>

          v .:? "lint_spaceBeforeComma" .!= lint_spaceBeforeComma defaultLintSettings <*>
          v .:? "lint_spaceAfterComma" .!= lint_spaceAfterComma defaultLintSettings <*>
          v .:? "lint_maxLineLength" .!= lint_maxLineLength defaultLintSettings <*>

          v .:? "lint_ignoreFiles" .!= lint_ignoreFiles defaultLintSettings <*>

          (v .:? "prettyprint_spaceBetweenParens" .!= prettyprint_spaceBetweenParens defaultLintSettings <|>
          v .:? "prettyprint_spaceAfterParens" .!= prettyprint_spaceBetweenParens defaultLintSettings) <*>

          (v .:? "prettyprint_spaceBetweenBrackets" .!= prettyprint_spaceBetweenBrackets defaultLintSettings <|>
          v .:? "prettyprint_spaceAfterBrackets" .!= prettyprint_spaceBetweenBrackets defaultLintSettings) <*>

          (v .:? "prettyprint_spaceBetweenBraces" .!= prettyprint_spaceBetweenBraces defaultLintSettings <|>
          v .:? "prettyprint_spaceAfterBraces" .!= prettyprint_spaceBetweenBraces defaultLintSettings) <*>

          v .:? "prettyprint_spaceEmptyParens" .!= prettyprint_spaceEmptyParens defaultLintSettings <*>
          v .:? "prettyprint_spaceEmptyBraces" .!= prettyprint_spaceEmptyBraces defaultLintSettings <*>
          v .:? "prettyprint_spaceAfterLabel" .!= prettyprint_spaceAfterLabel defaultLintSettings <*>
          v .:? "prettyprint_spaceBeforeComma" .!= prettyprint_spaceBeforeComma defaultLintSettings <*>
          v .:? "prettyprint_spaceAfterComma" .!= prettyprint_spaceAfterComma defaultLintSettings <*>
          v .:? "prettyprint_semicolons" .!= prettyprint_semicolons defaultLintSettings <*>
          v .:? "prettyprint_cStyle" .!= prettyprint_cStyle defaultLintSettings <*>
          v .:? "prettyprint_removeRedundantParens" .!= prettyprint_removeRedundantParens defaultLintSettings <*>
          v .:? "prettyprint_minimizeParens" .!= prettyprint_minimizeParens defaultLintSettings <*>
          v .:? "prettyprint_assumeOperatorAssociativity" .!= prettyprint_assumeOperatorAssociativity defaultLintSettings <*>
          v .:? "prettyprint_rejectInvalidCode" .!= prettyprint_rejectInvalidCode defaultLintSettings <*>
          v .:? "prettyprint_indentation" .!= prettyprint_indentation defaultLintSettings <*>
          v .:? "log_format" .!= log_format defaultLintSettings

    parseJSON _          = mzero

lint2ppSetting :: LintSettings -> PrettyPrintConfig
lint2ppSetting ls =
    PPConfig
    { spaceAfterParens = prettyprint_spaceBetweenParens ls
    , spaceAfterBrackets = prettyprint_spaceBetweenBrackets ls
    , spaceAfterBraces = prettyprint_spaceBetweenBraces ls
    , spaceEmptyParens = prettyprint_spaceEmptyParens ls
    , spaceEmptyBraces = prettyprint_spaceEmptyBraces ls
    , spaceAfterLabel = prettyprint_spaceAfterLabel ls
    , spaceBeforeComma = prettyprint_spaceBeforeComma ls
    , spaceAfterComma = prettyprint_spaceAfterComma ls
    , semicolons = prettyprint_semicolons ls
    , cStyle = prettyprint_cStyle ls
    , removeRedundantParens = prettyprint_removeRedundantParens ls
    , minimizeParens = prettyprint_minimizeParens ls
    , assumeOperatorAssociativity = prettyprint_assumeOperatorAssociativity ls
    , indentation = prettyprint_indentation ls
    }

instance ToJSON LintSettings where
    toJSON ls = object
        [ "lint_maxScopeDepth" .= lint_maxScopeDepth ls
        , "lint_syntaxErrors" .= lint_syntaxErrors ls
        , "lint_syntaxInconsistencies" .= lint_syntaxInconsistencies ls
        , "lint_deprecated" .= lint_deprecated ls
        , "lint_trailingWhitespace" .= lint_trailingWhitespace ls
        , "lint_whitespaceStyle" .= lint_whitespaceStyle ls
        , "lint_beginnerMistakes" .= lint_beginnerMistakes ls
        , "lint_emptyBlocks" .= lint_emptyBlocks ls
        , "lint_shadowing" .= lint_shadowing ls
        , "lint_gotos" .= lint_gotos ls
        , "lint_goto_identifier" .= lint_goto_identifier ls
        , "lint_doubleNegations" .= lint_doubleNegations ls
        , "lint_redundantIfStatements" .= lint_redundantIfStatements ls
        , "lint_redundantParentheses" .= lint_redundantParentheses ls
        , "lint_duplicateTableKeys" .= lint_duplicateTableKeys ls
        , "lint_profanity" .= lint_profanity ls
        , "lint_unusedVars" .= lint_unusedVars ls
        , "lint_unusedParameters" .= lint_unusedParameters ls
        , "lint_unusedLoopVars" .= lint_unusedLoopVars ls
        , "lint_inconsistentVariableStyle" .= lint_inconsistentVariableStyle ls
        , "lint_spaceBetweenParens" .= lint_spaceBetweenParens ls
        , "lint_spaceBetweenBrackets" .= lint_spaceBetweenBrackets ls
        , "lint_spaceBetweenBraces" .= lint_spaceBetweenBraces ls
        , "lint_maxLineLength" .= lint_maxLineLength ls
        , "lint_ignoreFiles" .= lint_ignoreFiles ls
        , "prettyprint_spaceBetweenParens" .= prettyprint_spaceBetweenParens ls
        , "prettyprint_spaceBetweenBrackets" .= prettyprint_spaceBetweenBrackets ls
        , "prettyprint_spaceBetweenBraces" .= prettyprint_spaceBetweenBraces ls
        , "prettyprint_spaceEmptyParens" .= prettyprint_spaceEmptyParens ls
        , "prettyprint_spaceEmptyBraces" .= prettyprint_spaceEmptyBraces ls
        , "prettyprint_spaceAfterLabel" .= prettyprint_spaceAfterLabel ls
        , "prettyprint_spaceBeforeComma" .= prettyprint_spaceBeforeComma ls
        , "prettyprint_spaceAfterComma" .= prettyprint_spaceAfterComma ls
        , "prettyprint_semicolons" .= prettyprint_semicolons ls
        , "prettyprint_cStyle" .= prettyprint_cStyle ls
        , "prettyprint_removeRedundantParens" .= prettyprint_removeRedundantParens ls
        , "prettyprint_minimizeParens" .= prettyprint_minimizeParens ls
        , "prettyprint_assumeOperatorAssociativity" .= prettyprint_assumeOperatorAssociativity ls
        , "prettyprint_rejectInvalidCode" .= prettyprint_rejectInvalidCode ls
        , "prettyprint_indentation" .= prettyprint_indentation ls
        , "log_format" .= log_format ls
        ]
