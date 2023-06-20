{-# LANGUAGE OverloadedRecordDot #-}

module GLuaFixer.Interface where

import GLua.AG.AST (AST)
import GLua.AG.PrettyPrint (prettyprintConf)
import GLua.AG.Token (MToken, Region)
import qualified GLua.Lexer as UUL
import GLua.LineLimitParser (LineLimit (..), execParseLineLimits)
import qualified GLua.PSLexer as PSL
import qualified GLua.PSParser as PSP
import qualified GLua.Parser as UUP
import GLuaFixer.AG.ASTLint (astWarnings)
import GLuaFixer.AG.LexLint (fixedLexPositions, lintWarnings)
import GLuaFixer.BadSequenceFinder (sequenceWarnings)
import GLuaFixer.LintMessage (Issue (..), LintMessage (..), Severity (..))
import GLuaFixer.LintSettings (LintSettings (..), lint2ppSetting)
import Text.Parsec.Error (errorPos)
import Text.ParserCombinators.UU.BasicInstances (Error, LineColPos)

-- | Run the linters that apply to the source code of a file
sourceLint :: LintSettings -> FilePath -> String -> [LintMessage]
sourceLint lintSettings filepath contents =
  execParseLineLimits filepath (LineLimit $ lint_maxLineLength lintSettings) contents

-- | Generate the lexicon of a source file, used as input for the parser
lex :: LintSettings -> FilePath -> String -> Either [LintMessage] [MToken]
lex lintSettings filepath contents =
  case PSL.execParseTokens contents of
    Left lexErr ->
      Left [LintMessage LintError (PSL.sp2Rg $ errorPos lexErr) (IssueParseError lexErr) filepath | lintSettings.lint_syntaxErrors]
    Right tokens -> Right $ fixedLexPositions tokens

-- | Use the (slower, but error-correcting) UU-parsinglib lexer to generate the lexicon
lexUU :: LintSettings -> String -> ([MToken], [Error LineColPos])
lexUU lintSettings contents = case UUL.execParseTokens contents of
  (tokens, errors) ->
    ( fixedLexPositions tokens
    , [err | lintSettings.lint_syntaxErrors, err <- errors]
    )

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

-- | Parse a lexicon into an Abstract Syntax Tree using the slower, but error-correcting
-- uu-parsinglib
parseUU :: [MToken] -> (AST, [Error Region])
parseUU = UUP.parseGLua

-- | Run the linters that inspect the AST
astLint :: FilePath -> LintSettings -> AST -> [LintMessage]
astLint filepath lintSettings ast =
  map ($ filepath) $ astWarnings lintSettings ast

-- | Pretty print code to a string
prettyprint :: LintSettings -> AST -> String
prettyprint lintSettings ast =
  prettyprintConf (lint2ppSetting lintSettings) ast
