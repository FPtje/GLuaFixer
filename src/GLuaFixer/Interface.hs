module GLuaFixer.Interface where

import GLua.AG.AST (AST)
import GLua.AG.PrettyPrint (prettyprintConf)
import GLua.AG.Token (MToken)
import GLua.LineLimitParser (LineLimit (..), execParseLineLimits)
import qualified GLua.PSLexer as PSL
import qualified GLua.PSParser as PSP
import GLuaFixer.AG.ASTLint (astWarnings)
import GLuaFixer.AG.LexLint (fixedLexPositions, lintWarnings)
import GLuaFixer.BadSequenceFinder (sequenceWarnings)
import GLuaFixer.LintMessage (Issue (..), LintMessage (..), Severity (..))
import GLuaFixer.LintSettings (LintSettings (..), lint2ppSetting)
import Text.Parsec.Error (errorPos)

-- | Run the linters that apply to the source code of a file
sourceLint :: LintSettings -> FilePath -> String -> [LintMessage]
sourceLint lintSettings filepath contents =
  execParseLineLimits filepath (LineLimit $ lint_maxLineLength lintSettings) contents

-- | Generate the lexicon of a source file, used as input for the parser
lex :: LintSettings -> FilePath -> String -> Either [LintMessage] [MToken]
lex lintSettings filepath contents =
  case PSL.execParseTokens contents of
    Left lexErr ->
      Left [LintMessage LintError (PSL.sp2Rg $ errorPos lexErr) (IssueParseError lexErr) filepath | lint_syntaxErrors lintSettings]
    Right tokens -> Right $ fixedLexPositions tokens

-- | Run the linting functions that inspect the lexicon
lexiconLint :: FilePath -> LintSettings -> [MToken] -> [LintMessage]
lexiconLint filepath lintSettings tokens =
  map ($ filepath) $
    lintWarnings lintSettings tokens ++ sequenceWarnings lintSettings tokens

-- | Parse a lexicon into an Abstract Syntax Tree
parse :: LintSettings -> FilePath -> [MToken] -> Either [LintMessage] AST
parse lintSettings filepath tokens =
  case PSP.parseGLua tokens of
    Left err ->
      -- Return syntax errors
      Left [LintMessage LintError (PSL.sp2Rg $ errorPos err) (IssueParseError err) filepath | lint_syntaxErrors lintSettings]
    Right ast -> Right ast

-- | Run the linters that inspect the AST
astLint :: FilePath -> LintSettings -> AST -> [LintMessage]
astLint filepath lintSettings ast =
  map ($ filepath) $ astWarnings lintSettings ast

-- | Pretty print code to a string
prettyprint :: LintSettings -> AST -> String
prettyprint lintSettings ast =
  prettyprintConf (lint2ppSetting lintSettings) ast
