{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module GLuaFixer.LintMessage where

import Control.Monad
import Data.Aeson
import Data.List (sortOn)
import Data.Maybe (isJust)
import System.Environment (lookupEnv)
import Text.Parsec (ParseError)
import Text.ParserCombinators.UU.BasicInstances hiding (msgs)

import GLua.AG.Token
import GLua.AG.PrettyPrint

-- | Output formats for logging
data LogFormat = StandardLogFormat | GithubLogFormat
data LogFormatChoice = AutoLogFormatChoice | LogFormatChoice !LogFormat

instance Show LogFormat where
  show StandardLogFormat = "standard"
  show GithubLogFormat = "github"

instance Show LogFormatChoice where
  show (LogFormatChoice choice) = show choice
  show AutoLogFormatChoice = "auto"

instance ToJSON LogFormat where
  toJSON StandardLogFormat = "standard"
  toJSON GithubLogFormat = "github"

instance ToJSON LogFormatChoice where
  toJSON (LogFormatChoice choice) = toJSON choice
  toJSON AutoLogFormatChoice = "auto"

instance FromJSON LogFormatChoice where
  parseJSON (String logFormat) = case logFormat of
    "standard" -> pure $ LogFormatChoice StandardLogFormat
    "github" -> pure $ LogFormatChoice GithubLogFormat
    "auto" -> pure AutoLogFormatChoice
    _ -> fail ( "Please use either \"auto\" \"standard\" or \"github\" but was " ++ show logFormat )
  parseJSON _ = mzero

data Severity = LintWarning | LintError
  deriving (Eq)

-- | Representation of the different kinds of issues that can be raised. Many of the arguments are
-- 'String', because this data type is a rewrite of what was previously directly rendered Strings.
-- Many of these Strings can later be rewritten to their own types if necessary.
data Issue
  = IssueParseError ParseError

    -- From BadSequenceFinder
  | Deprecated !String -- ^ Reason
  | Profanity
  | BeginnerMistake !String -- ^ message
  | WhitespaceStyle !String -- ^ message

  -- Issues found in the lexicon (see LexLint.ag)
  | TrailingWhitespace
  | InconsistentTabsSpaces
  | SyntaxInconsistency
    !String -- ^ First encountered
    !String -- ^ Second encountered

  -- Issues found in the AST (see ASTLint.ag)
  | VariableShadows
    !String -- ^ Name of the variable being shadowed
    !Region -- ^ Definition location of variable being shadowed
  | GotoAsIdentifier
  | InconsistentVariableNaming
  | ScopePyramids
  | UnusedVariable !String -- ^ Variable name
  | AvoidGoto
  | EmptyDoBlock
  | EmptyWhileLoop
  | EmptyRepeat
  | EmptyIf
  | DoubleIf
  | EmptyFor
  | EmptyElseIf
  | EmptyElse
  | SelfInNonMeta
  | SelfEntity
  | SelfWeapon
  | UnnecessaryParentheses
  | SillyNegation !String -- ^ Alternative to using the negation
  | DuplicateKeyInTable !Token -- ^ The key that is duplicated
  deriving Eq

-- | Represents lint messages
data LintMessage
  = LintMessage
    { lintmsg_severity :: !Severity
    , lintmsg_region   :: !Region
    , lintmsg_message  :: !Issue
    , lintmsg_file     :: !FilePath
    }
  deriving (Eq)

instance Show LintMessage where
    show lintMsg = formatLintMessageDefault lintMsg

issueDescription :: Issue -> String
issueDescription = \case
  IssueParseError parseError -> renderPSError parseError

  Deprecated reason -> "Deprecated: " ++ reason
  Profanity -> "Watch your profanity"
  BeginnerMistake msg -> msg
  WhitespaceStyle msg -> "Style: " ++ msg

  TrailingWhitespace -> "Trailing whitespace"
  InconsistentTabsSpaces -> "Inconsistent use of tabs and spaces for indentation"
  SyntaxInconsistency firstEncountered secondEncountered ->
    "Inconsistent use of '" ++ firstEncountered ++ "' and '" ++ secondEncountered ++ "'"

  VariableShadows lbl (Region start _) ->
    "Variable '" ++ lbl ++ "' shadows existing binding, defined at " ++ renderPos start
  GotoAsIdentifier ->
    "Don't use 'goto' as an identifier, later versions of Lua will confuse it with the goto keyword."
  InconsistentVariableNaming ->
    "Inconsistent variable naming! There are variables that start with a lowercase letter, as well as ones that start with an uppercase letter. Please decide on one style."
  ScopePyramids ->
    "Are you Egyptian? What's with these fucking scope pyramids!?"
  UnusedVariable varName ->
    "Unused variable: " ++ varName
  AvoidGoto ->
    "Don't use labels and gotos unless you're jumping out of multiple loops."
  EmptyDoBlock -> "Empty do block"
  EmptyWhileLoop -> "Empty while loop"
  EmptyRepeat -> "Empty repeat statement"
  EmptyIf -> "Empty if statement"
  DoubleIf ->
    "Double if statement. Please combine the condition of this if statement with that of the outer if statement using `and`."
  EmptyFor -> "Empty for loop"
  EmptyElseIf -> "Empty elseif statement"
  EmptyElse -> "Empty else statement"
  SelfInNonMeta ->
    "Don't use self in a non-metafunction"
  SelfEntity ->
    "'self.Entity' is the same as just 'self' in SENTs"
  SelfWeapon ->
    "'self.Weapon' is the same as just 'self' in SWEPs"
  UnnecessaryParentheses -> "Unnecessary parentheses"
  SillyNegation alternative ->
    "Silly negation. Use '" ++ alternative ++ "'"
  DuplicateKeyInTable keyToken ->
    "Duplicate key in table: '" ++ show keyToken ++ "'."


logFormatChoiceToLogFormat :: LogFormatChoice -> IO LogFormat
logFormatChoiceToLogFormat = \case
  LogFormatChoice format -> pure format
  AutoLogFormatChoice -> do
    actionsExists <- isJust <$> lookupEnv "GITHUB_ACTIONS"
    workflowExists <- isJust <$> lookupEnv "GITHUB_WORKFLOW"
    if actionsExists && workflowExists
    then pure GithubLogFormat
    else pure StandardLogFormat


formatLintMessage :: LogFormat -> LintMessage -> String
formatLintMessage StandardLogFormat lintMsg = formatLintMessageDefault lintMsg
formatLintMessage GithubLogFormat lintMsg = formatLintMessageGithub lintMsg

formatLintMessageDefault :: LintMessage -> String
formatLintMessageDefault (LintMessage severity region msg file) =
  let
    level = case severity of
      LintWarning -> "Warning"
      LintError -> "Error"
  in
    showString file .
    showString ": [" . showString level . showString "] " .
    showString (renderRegion region) . showString ": " .
    showString (issueDescription msg) $
    ""

formatLintMessageGithub :: LintMessage -> String
formatLintMessageGithub (LintMessage severity (Region (LineColPos line col _) (LineColPos endLine endCol _)) msg file) =
  let
    level = case severity of
      LintWarning -> "warning"
      LintError -> "error"
  in
    showString "::" . showString level .
    showString " file=" . showString file .
    showString ",line=" . shows (succ line) .
    showString ",col=" . shows (succ col) .
    showString ",endLine=" . shows (succ endLine) .
    showString ",endColumn=" . shows (succ endCol) .
    showString "::" . showString (issueDescription msg) $
    ""

-- | Sort lint messages on file and then region
sortLintMessages :: [LintMessage] -> [LintMessage]
sortLintMessages msgs =
    sortOn (\(LintMessage _ rg _ f) -> (f, rg)) msgs
