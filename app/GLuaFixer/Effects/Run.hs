{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GLuaFixer.Effects.Run where

import Control.Monad (unless, when)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BL8
import Effectful (Eff, (:>))
import qualified Effectful.Environment as Eff
import Effectful.Error.Static (runErrorNoCallStack)
import GLua.AG.AST (AST)
import GLua.AG.Token (MToken)
import GLua.ASTInstances ()
import qualified GLua.PSParser as PSP
import qualified GLua.Parser as UUP
import GLuaFixer.Cli (Command (..), Options (..), OverriddenSettings, SettingsPath)
import GLuaFixer.Effects.AnalyseGlobals (analyseFile, execAnalysis, reportAnalysis)
import GLuaFixer.Effects.Cli (Cli, CliParseResult (..), parseCliOptions)
import GLuaFixer.Effects.Files (Files, IgnoreFiles (..), findLuaFiles, getCurrentDirectory, isDirectory, readFile, readStdIn, traceFilesIfEnabled, writeFile)
import GLuaFixer.Effects.Interruptible (Interruptible, interruptibleFoldMStrict)
import GLuaFixer.Effects.Logging (Logging, emitLintMessage, getLogFormat, putStrLnStdError, putStrLnStdOut, putStrStdOut)
import GLuaFixer.Effects.Settings (Settings, SettingsError (CouldNotParseSettings), getSettingsForFile, runSettings, traceSettingsIfEnabled)
import qualified GLuaFixer.Interface as Interface
import GLuaFixer.LintMessage (sortLintMessages)
import GLuaFixer.LintSettings (
  LintSettings (..),
  StdInOrFiles (..),
 )
import GLuaFixer.Version (version)
import System.Exit (ExitCode (..))
import Prelude hiding (lex, readFile, writeFile)

-- | Top level run function
run
  :: ( Interruptible :> es
     , Files :> es
     , Logging :> es
     , Eff.Environment :> es
     , Cli :> es
     )
  => Eff es ExitCode
run = do
  pareResult <- parseCliOptions

  case pareResult of
    PrintHelpText exitCode helpText -> do
      putStrLnStdError helpText
      pure exitCode
    ParseSuccessful options -> do
      result <-
        runErrorNoCallStack @SettingsError $
          runSettings $
            runOptions options

      case result of
        Left (CouldNotParseSettings err) -> do
          putStrLnStdError $ "Could not parse settings file: " <> err
          pure $ ExitFailure 1
        Right exitCode -> pure exitCode

-- | Run the given options
runOptions
  :: ( Interruptible :> es
     , Files :> es
     , Settings :> es
     , Logging :> es
     , Eff.Environment :> es
     , Cli :> es
     )
  => Options
  -> Eff es ExitCode
runOptions options =
  traceFilesIfEnabled options.optsDebug $
    traceSettingsIfEnabled options.optsDebug $ do
      when (options.optsDebug) $
        putStrLnStdError $
          show options
      case (options.optsCommand, options.optsFiles) of
        (Lint, UseStdIn) -> do
          (lintSettings, contents) <- getStdIn options.optsConfigFile options.optsOverridden
          lint lintSettings "stdin" contents
        (Lint, UseFiles files) ->
          foldLuaFiles
            options.optsConfigFile
            options.optsOverridden
            ExitSuccess
            files
            $ \exitCode lintSettings filepath contents ->
              worstExitCode exitCode <$> lint lintSettings filepath contents
        (PrettyPrint, UseStdIn) -> do
          (lintSettings, contents) <- getStdIn options.optsConfigFile options.optsOverridden
          case prettyprint lintSettings contents of
            Nothing -> pure $ ExitFailure 1
            Just prettyprinted -> do
              putStrStdOut prettyprinted
              pure ExitSuccess
        (PrettyPrint, UseFiles files) ->
          foldLuaFiles
            options.optsConfigFile
            options.optsOverridden
            ExitSuccess
            files
            $ \exitCode lintSettings filepath contents -> do
              putStrLnStdOut $ "Pretty printing " <> filepath
              case prettyprint lintSettings contents of
                Nothing -> pure $ ExitFailure 1
                Just prettyprinted -> do
                  writeFile filepath prettyprinted
                  pure exitCode
        (AnalyseGlobals, UseStdIn) -> do
          (lintSettings, contents) <- getStdIn options.optsConfigFile options.optsOverridden
          withParsed lintSettings "stdin" contents (ExitFailure 1) $ \ast -> do
            analysis <- execAnalysis $ analyseFile lintSettings "stdin" ast
            reportAnalysis analysis
            pure ExitSuccess
        (AnalyseGlobals, UseFiles files) -> do
          analysis <- execAnalysis
            $ foldLuaFiles
              options.optsConfigFile
              options.optsOverridden
              ()
              files
            $ \() lintSettings filepath contents -> do
              withParsed lintSettings filepath contents () $ \ast ->
                analyseFile lintSettings filepath ast
          reportAnalysis analysis
          pure ExitSuccess
        (DumpLexicon, UseStdIn) -> do
          (lintSettings, contents) <- getStdIn options.optsConfigFile options.optsOverridden
          mbTokens <- getLexicon lintSettings "stdin" contents
          case mbTokens of
            Nothing -> pure $ ExitFailure 1
            tokens -> do
              putStrLnStdOut $ show tokens
              pure ExitSuccess
        (DumpLexicon, UseFiles files) ->
          foldLuaFiles
            options.optsConfigFile
            options.optsOverridden
            ExitSuccess
            files
            $ \exitCode lintSettings filepath contents -> do
              mbTokens <- getLexicon lintSettings filepath contents
              case mbTokens of
                Nothing -> pure $ ExitFailure 1
                Just tokens -> do
                  putStrLnStdOut $ show tokens
                  pure exitCode
        (DumpAst, UseStdIn) -> do
          (lintSettings, contents) <- getStdIn options.optsConfigFile options.optsOverridden
          withParsed lintSettings "stdin" contents (ExitFailure 1) $ \ast -> do
            putStrLnStdOut $ BL8.unpack $ JSON.encode ast
            pure ExitSuccess
        (DumpAst, UseFiles files) ->
          foldLuaFiles
            options.optsConfigFile
            options.optsOverridden
            ExitSuccess
            files
            $ \exitCode lintSettings filepath contents ->
              withParsed lintSettings filepath contents (ExitFailure 1) $ \ast -> do
                putStrLnStdOut $ BL8.unpack $ JSON.encode ast
                pure exitCode
        (Test, UseStdIn) -> do
          (lintSettings, contents) <- getStdIn options.optsConfigFile options.optsOverridden
          test ExitSuccess lintSettings "stdin" contents
        (Test, UseFiles files) -> do
          foldLuaFiles
            options.optsConfigFile
            options.optsOverridden
            ExitSuccess
            files
            $ \exitCode lintSettings filepath contents ->
              test exitCode lintSettings filepath contents
        (PrintVersion, _) -> do
          putStrLnStdOut version
          pure ExitSuccess

-- | Retrieves the contents of stdin and the settings that apply.
getStdIn
  :: (Files :> es, Settings :> es)
  => Maybe SettingsPath
  -> OverriddenSettings
  -> Eff es (LintSettings, String)
getStdIn mbSettingsPath overriddenSettings = do
  cwd <- getCurrentDirectory
  settings <- getSettingsForFile mbSettingsPath overriddenSettings cwd
  code <- readStdIn
  pure (settings, code)

-- | Fold over all the Lua files. Recurses into directories, retrieves settings and passes file
-- contents to the fold function.
foldLuaFiles
  :: forall es a
   . (Files :> es, Interruptible :> es, Settings :> es)
  => Maybe SettingsPath
  -> OverriddenSettings
  -> a
  -> [FilePath]
  -> (a -> LintSettings -> FilePath -> String -> Eff es a)
  -> Eff es a
foldLuaFiles mbSettingsPath overriddenSettings initial files f =
  interruptibleFoldMStrict go initial files
  where
    go :: a -> FilePath -> Eff es a
    go acc file = do
      isDir <- isDirectory file
      lintSettings <- getSettingsForFile mbSettingsPath overriddenSettings file
      if isDir
        then do
          let
            ignoreFiles = IgnoreFiles $ lintSettings.lint_ignoreFiles
          recurseFiles <- findLuaFiles ignoreFiles file
          interruptibleFoldMStrict go acc recurseFiles
        else do
          contents <- readFile file
          f acc lintSettings file contents

-- | Lint a file
lint
  :: (Logging :> es, Eff.Environment :> es)
  => LintSettings
  -> FilePath
  -> String
  -> Eff es ExitCode
lint lintSettings filepath contents = do
  logFormat <- getLogFormat lintSettings.log_format
  let
    sourceLint = Interface.sourceLint lintSettings filepath contents
  case Interface.lex lintSettings filepath contents of
    Left msgs -> do
      mapM_ (emitLintMessage logFormat) msgs
      pure $ ExitFailure 1
    Right tokens -> do
      let
        !lextLint = Interface.lexiconLint filepath lintSettings tokens
      case Interface.parse lintSettings filepath tokens of
        Left msgs -> do
          mapM_ (emitLintMessage logFormat) msgs
          pure $ ExitFailure 1
        Right ast -> do
          let
            astLint = Interface.astLint filepath lintSettings ast
            msgs = sortLintMessages $ sourceLint ++ lextLint ++ astLint
          mapM_ (emitLintMessage logFormat) msgs
          pure $ if null msgs then ExitSuccess else ExitFailure 1

-- | Pretty print a file
prettyprint
  :: LintSettings
  -> String
  -> Maybe String
prettyprint lintSettings contents = do
  if lintSettings.prettyprint_rejectInvalidCode && hasErrors
    then Nothing
    else Just $ Interface.prettyprint lintSettings ast
  where
    (tokens, lexErrors) = Interface.lexUU lintSettings contents
    (ast, parseErrors) = Interface.parseUU tokens
    hasErrors = not (null lexErrors) || not (null parseErrors)

-- | Test glualint itself against a file. TODO: Refactor this into a nicer command
test
  :: (Logging :> es, Eff.Environment :> es)
  => ExitCode
  -> LintSettings
  -> FilePath
  -> String
  -> Eff es ExitCode
test exitCode lintSettings filepath contents = do
  putStrLnStdOut $ "Testing " <> filepath
  let
    (uu_lex, uu_lex_errors) = Interface.lexUU lintSettings contents
    (_uu_ast, uu_parseErrs) = Interface.parseUU uu_lex

  unless (null uu_lex_errors) $ do
    putStrLnStdOut $
      "Errors when trying to lex '"
        ++ filepath
        ++ "' with uu-parsinglib lexer!"
    mapM_ (putStrLnStdOut . show) uu_lex_errors

  unless (null uu_parseErrs) $ do
    putStrLnStdOut $
      "Errors when trying to parse '"
        ++ filepath
        ++ "' with uu-parsinglib parser!"
    mapM_ (putStrLnStdOut . show) uu_parseErrs

  logFormat <- getLogFormat lintSettings.log_format

  case Interface.lex lintSettings filepath contents of
    Left msgs -> do
      mapM_ (emitLintMessage logFormat) msgs
      pure $ ExitFailure 1
    Right tokens ->
      case Interface.parse lintSettings filepath tokens of
        Left msgs -> do
          putStrLnStdOut $
            "Errors when trying to parse '" ++ filepath ++ "' with parsec parser!"
          mapM_ (emitLintMessage logFormat) msgs
          pure $ ExitFailure 1
        Right ast -> do
          let
            prettyprinted = Interface.prettyprint lintSettings ast
            (_uu_ast_pp, uu_parseErrs_pp) = UUP.parseGLuaFromString prettyprinted

          unless (null uu_parseErrs_pp) $ do
            putStrLnStdOut $
              "Errors when trying to parse '"
                ++ filepath
                ++ "' with uu-parsinglib parser after pretty print!"
            mapM_ (putStrLnStdOut . show) uu_parseErrs_pp

          case PSP.parseGLuaFromString prettyprinted of
            Left err -> do
              putStrLnStdOut $
                "Errors when trying to parse '" ++ filepath ++ "' with parsec parser after pretty print!"

              putStrLnStdOut $ show err
              pure $ ExitFailure 1
            Right _ast -> pure exitCode

-- | Function to easily parse a file's contents into an AST. This will log any parse failures and
-- give an AST if it can.
withParsed
  :: (Logging :> es, Eff.Environment :> es)
  => LintSettings
  -> FilePath
  -> String
  -> a
  -> (AST -> Eff es a)
  -> Eff es a
withParsed lintSettings filepath contents defaultValue f = do
  lexicon <- getLexicon lintSettings filepath contents
  case lexicon of
    Nothing -> pure defaultValue
    Just tokens -> do
      logFormat <- getLogFormat lintSettings.log_format
      case Interface.parse lintSettings filepath tokens of
        Left msgs -> do
          mapM_ (emitLintMessage logFormat) msgs
          pure defaultValue
        Right ast -> f ast

-- | Function to parse a file's contents into MTokens. This will log any parse failures and give
-- MTokens if it can.
getLexicon :: (Logging :> es) => LintSettings -> FilePath -> String -> Eff es (Maybe [MToken])
getLexicon lintSettings filepath contents = do
  logFormat <- getLogFormat lintSettings.log_format
  case Interface.lex lintSettings filepath contents of
    Left msgs -> do
      mapM_ (emitLintMessage logFormat) msgs
      pure Nothing
    Right tokens -> do
      pure $ Just tokens

-- | Takes the worst of the two exit codes.
worstExitCode :: ExitCode -> ExitCode -> ExitCode
worstExitCode e1 e2 = case (e1, e2) of
  (ExitSuccess, ExitSuccess) -> ExitSuccess
  (ExitFailure code, _) -> ExitFailure code
  (_, ExitFailure code) -> ExitFailure code
