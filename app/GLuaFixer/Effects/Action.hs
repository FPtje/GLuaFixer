{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GLuaFixer.Effects.Action where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BL8
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import qualified Effectful.Environment as Eff
import Effectful.TH (makeEffect)
import GLua.AG.AST (AST)
import GLua.ASTInstances ()
import GLuaFixer.Cli (Command (..), Options (..), OverriddenSettings, SettingsPath)
import GLuaFixer.Effects.Cli (Cli, CliParseResult (..), parseCliOptions)
import GLuaFixer.Effects.Files (Files, IgnoreFiles (..), findLuaFiles, getCurrentDirectory, isDirectory, readFile, readStdIn, writeFile)
import GLuaFixer.Effects.Interruptible (Interruptible, interruptibleFoldMStrict)
import GLuaFixer.Effects.Logging (Logging, emitLintMessage, getLogFormat, putStrLnStdError, putStrLnStdOut, putStrStdOut)
import GLuaFixer.Effects.Settings (Settings, getSettingsForFile)
import qualified GLuaFixer.Interface as Interface
import GLuaFixer.LintMessage (sortLintMessages)
import GLuaFixer.LintSettings (
  LintSettings (..),
  StdInOrFiles (..),
 )
import System.Exit (ExitCode (..))
import Prelude hiding (lex, readFile, writeFile)

version :: String
version = "1.24.3"

data Action :: Effect where
  Run :: Action m ExitCode
  RunOptions :: Options -> Action m ExitCode

type instance DispatchOf Action = Dynamic

makeEffect ''Action

runAction ::
  ( Interruptible :> es
  , Files :> es
  , Settings :> es
  , Logging :> es
  , Eff.Environment :> es
  , Cli :> es
  ) =>
  Eff (Action : es) a ->
  Eff es a
runAction = interpret $ \_ -> \case
  Run -> do
    pareResult <- parseCliOptions

    case pareResult of
      PrintHelpText exitCode helpText -> do
        putStrLnStdError helpText
        pure exitCode
      ParseSuccessful options -> runAction $ runOptions options
  RunOptions options -> case (options.optsCommand, options.optsFiles) of
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
      -- TODO
      pure $ ExitFailure 1
    (AnalyseGlobals, UseFiles _files) ->
      -- TODO
      pure $ ExitFailure 1
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
      -- TODO
      pure $ ExitFailure 1
    (Test, UseFiles _files) ->
      -- TODO
      pure $ ExitFailure 1
    (PrintVersion, _) -> do
      putStrLnStdOut version
      pure ExitSuccess

-- | Retrieves the contents of stdin and the settings that apply.
getStdIn ::
  (Files :> es, Settings :> es) =>
  Maybe SettingsPath ->
  OverriddenSettings ->
  Eff es (LintSettings, String)
getStdIn mbSettingsPath overriddenSettings = do
  cwd <- getCurrentDirectory
  settings <- getSettingsForFile mbSettingsPath overriddenSettings cwd
  code <- readStdIn
  pure (settings, code)

{- | Fold over all the Lua files. Recurses into directories, retrieves settings and passes file
contents to the fold function.
-}
foldLuaFiles ::
  forall es a.
  (Files :> es, Interruptible :> es, Settings :> es) =>
  Maybe SettingsPath ->
  OverriddenSettings ->
  a ->
  [FilePath] ->
  (a -> LintSettings -> FilePath -> String -> Eff es a) ->
  Eff es a
foldLuaFiles mbSettingsPath overriddenSettings initial files f =
  interruptibleFoldMStrict go initial files
 where
  go :: a -> FilePath -> Eff es a
  go acc file = do
    isDir <- isDirectory file
    lintSettings <- getSettingsForFile mbSettingsPath overriddenSettings file
    if isDir
      then do
        let ignoreFiles = IgnoreFiles $ lintSettings.lint_ignoreFiles
        recurseFiles <- findLuaFiles ignoreFiles file
        interruptibleFoldMStrict go acc recurseFiles
      else do
        contents <- readFile file
        f acc lintSettings file contents

lint ::
  (Logging :> es, Eff.Environment :> es) =>
  LintSettings ->
  FilePath ->
  String ->
  Eff es ExitCode
lint lintSettings filepath contents = do
  logFormat <- getLogFormat lintSettings.log_format
  let sourceLint = Interface.sourceLint lintSettings filepath contents
  case Interface.lex lintSettings filepath contents of
    Left msgs -> do
      mapM_ (emitLintMessage logFormat) msgs
      pure $ ExitFailure 1
    Right tokens -> do
      let !lextLint = Interface.lexiconLint filepath lintSettings tokens
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

prettyprint ::
  LintSettings ->
  String ->
  Maybe String
prettyprint lintSettings contents = do
  if lintSettings.prettyprint_rejectInvalidCode && hasErrors
    then Nothing
    else Just $ Interface.prettyprint lintSettings ast
 where
  (tokens, lexErrors) = Interface.lexUU lintSettings contents
  (ast, parseErrors) = Interface.parseUU tokens
  hasErrors = not (null lexErrors) || not (null parseErrors)

withParsed ::
  (Logging :> es, Eff.Environment :> es) =>
  LintSettings ->
  FilePath ->
  String ->
  a ->
  (AST -> Eff es a) ->
  Eff es a
withParsed lintSettings filepath contents defaultValue f = do
  logFormat <- getLogFormat lintSettings.log_format
  case Interface.lex lintSettings filepath contents of
    Left msgs -> do
      mapM_ (emitLintMessage logFormat) msgs
      pure defaultValue
    Right tokens -> do
      case Interface.parse lintSettings filepath tokens of
        Left msgs -> do
          mapM_ (emitLintMessage logFormat) msgs
          pure defaultValue
        Right ast -> f ast

-- | Takes the worst of the two exit codes.
worstExitCode :: ExitCode -> ExitCode -> ExitCode
worstExitCode e1 e2 = case (e1, e2) of
  (ExitSuccess, ExitSuccess) -> ExitSuccess
  (ExitFailure code, _) -> ExitFailure code
  (_, ExitFailure code) -> ExitFailure code
