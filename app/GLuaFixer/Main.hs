{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import GLua.AG.PrettyPrint ( prettyprintConf )
import GLua.Parser ( parseGLua, parseGLuaFromString )
import GLua.ASTInstances ()
import GLua.LineLimitParser (execParseLineLimits, LineLimit (LineLimit))
import GLua.TokenTypes (isWhitespace)
import GLuaFixer.AG.ASTLint ( astWarnings )
import GLuaFixer.AnalyseProject ( analyseGlobals )
import GLuaFixer.Cli (Options (..), Command (..), OverriddenSettings (..), runParse)
import GLuaFixer.LintMessage
    ( LintMessage,
      formatLintMessage,
      logFormatChoiceToLogFormat,
      sortLintMessages,
      LogFormat(StandardLogFormat, GithubLogFormat) )
import GLuaFixer.LintSettings
    ( Indentation(..),
      StdInOrFiles(..),
      LintSettings(lint_ignoreFiles, prettyprint_rejectInvalidCode,
                   lint_maxLineLength, prettyprint_indentation, log_format),
      lint2ppSetting )
import GLuaFixer.Util
    ( doReadFile,
      doWriteFile,
      findLuaFiles,
      forEachInput,
      forEachInput_,
      getSettings,
      parseFile,
      settingsFromFile,
      Abort(..) )

import qualified GLua.Lexer as Lex

import Control.Applicative ((<|>))
import Control.Monad (unless, void)
import Data.Functor ((<&>))
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.Maybe (fromJust)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure, ExitCode (..))
import System.IO (hPutStrLn, stderr)
import Data.Foldable ( for_ )

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BL
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help.Types as Opt
import qualified System.Signal as Signal


version :: String
version = "1.24.3"

main :: IO ()
main = do
  -- Set the encoding of the program to UTF-8. This is to prevent problems when the system's locale
  -- is set to anything but UTF-8.
  -- See https://github.com/FPtje/GLuaFixer/issues/145
  setLocaleEncoding utf8
  -- Keep track of whether process is cancelled through some signal
  aborted <- newIORef Continue

  Signal.installHandler Signal.sigTERM $ \_ -> atomicWriteIORef aborted Abort
  Signal.installHandler Signal.sigINT $ \_ -> atomicWriteIORef aborted Abort

  args <- getArgs
  let cliParseResult = runParse args

  case runParse args of
    Opt.Success options -> runGluaLint options aborted
    Opt.CompletionInvoked _completionResult -> void $ Opt.handleParseResult cliParseResult
    Opt.Failure parserFailure -> do
      progName <- getProgName
      handleCliFailure aborted args $ Opt.execFailure parserFailure progName

-- | Run subcommands
runGluaLint :: Options -> IORef Abort -> IO ()
runGluaLint opts aborted = do
    case optsCommand opts of
      Lint -> do
        let stdinOrFiles = optsFiles opts
        noErrorsOrWarnings <- and <$> forEachInput mbIndent mbOutputFormat stdinOrFiles aborted
          lintStdin
          lintFile

        unless noErrorsOrWarnings exitFailure
      PrettyPrint -> do
        let stdinOrFiles = optsFiles opts
        noErrors <- and <$> forEachInput mbIndent mbOutputFormat stdinOrFiles aborted
          prettyPrintStdin
          prettyPrintFile

        unless noErrors exitFailure
      AnalyseGlobals ->
        case optsFiles opts of
          UseFiles files -> analyseGlobals files
          UseStdIn -> pure ()
      DumpAst -> do
        let stdinOrFiles = optsFiles opts
        forEachInput_ mbIndent mbOutputFormat stdinOrFiles aborted
          dumpASTStdin
          dumpASTFile
      Test -> do
        case optsFiles opts of
          UseFiles files -> runTest files
          UseStdIn -> pure ()
      PrintVersion ->
        putStrLn version
  where
    mbIndent = opts.optsOverridden.indentation
    mbOutputFormat = opts.optsOverridden.outputFormat

-- | When the regulare CLI fails to parse, the legacy one might yet succeed
handleCliFailure :: IORef Abort -> [String] -> (Opt.ParserHelp, ExitCode, Int) -> IO ()
handleCliFailure aborted args (parserHelp, exitCode, terminalColumns) = case exitCode of
  -- This means the help was activated. Print the help
  ExitSuccess -> putStrLn $ Opt.renderHelp terminalColumns parserHelp
  ExitFailure _ -> do
    -- Attempt legacy CLI interface
    mbRes <- legacyCli aborted Nothing args
    case mbRes of
      -- Legacy parser failed as well, print help
      Nothing -> do
        hPutStrLn stderr $ Opt.renderHelp terminalColumns parserHelp
        exitFailure
      -- Empty file list, print help
      Just (_settings, []) -> do
        hPutStrLn stderr $ Opt.renderHelp terminalColumns parserHelp
        exitFailure
      Just (settings, files) -> do
        hasMessages <- legacyLint settings files
        if hasMessages then exitFailure
        else exitSuccess

prettyPrintStdin :: LintSettings -> String -> IO Bool
prettyPrintStdin settings contents =
  case prettyPrint settings contents of
    Nothing -> pure False
    Just result -> True <$ putStr result

prettyPrintFile :: LintSettings -> FilePath -> String -> IO Bool
prettyPrintFile settings filePath contents = do
  hPutStrLn stderr $ "Pretty printing " ++ filePath
  case prettyPrint settings contents of
    Nothing -> pure False
    Just result -> True <$ doWriteFile filePath result

-- | Pure pretty print function
prettyPrint :: LintSettings -> String -> Maybe String
prettyPrint lintsettings lua =
    if prettyprint_rejectInvalidCode lintsettings && not (null errors)
    then Nothing
    else Just $ prettyprintConf ppconf ast
  where
    (ast, errors) = parseGLuaFromString lua
    ppconf = lint2ppSetting lintsettings

-- | Lint from stdin
lintStdin :: LintSettings -> String -> IO Bool
lintStdin settings contents = do
    lintFile settings "stdin" contents

-- | Lint a file
lintFile :: LintSettings -> FilePath -> String -> IO Bool
lintFile settings filePath contents = do
    let msgs = lint settings filePath contents
    logFormat <- logFormatChoiceToLogFormat $ log_format settings
    case logFormat of
      StandardLogFormat ->
        mapM_ (putStrLn . formatLintMessage StandardLogFormat) msgs
      GithubLogFormat -> do
        -- When the log format is GitHub, also print in the regular format. GitHub actions hide the
        -- GitHub specific output from the logs for some reason.
        mapM_ (putStrLn . formatLintMessage GithubLogFormat) msgs
        mapM_ (putStrLn . formatLintMessage StandardLogFormat) msgs
    pure $ null msgs

-- | Lint a string, using parsec
lint :: LintSettings -> FilePath -> String -> [LintMessage]
lint config f contents =
    case parseFile config f contents of
      Left errs -> errs
      Right (lexWarnings, ast) ->
        let
            lineLengthWarnings = execParseLineLimits f (LineLimit $ lint_maxLineLength config) contents
            parserWarnings = map ($ f) $ astWarnings config ast
        in
            -- Print all warnings
            sortLintMessages $ lineLengthWarnings ++ lexWarnings ++ parserWarnings

-- | Pretty print, uses the uu-parsinglib library
legacyPrettyPrintStdin :: Maybe Indentation -> IO ()
legacyPrettyPrintStdin ind = do
    lua <- getContents

    cwd <- getCurrentDirectory
    lintsettings <- overrideLintSettingsIndentation ind <$> getSettings cwd

    for_ (prettyPrint lintsettings lua) $ \result ->
        putStr result

legacyPrettyPrintFiles :: IORef Abort -> Maybe Indentation -> [FilePath] -> IO ()
legacyPrettyPrintFiles aborted ind = mapM_ pp
  where
    pp :: FilePath -> IO ()
    pp f = do
      isDirectory <- doesDirectoryExist f
      if isDirectory then do
        luaFiles <- findLuaFiles [] f
        legacyPrettyPrintFiles aborted ind luaFiles
      else do
        lintsettings <- overrideLintSettingsIndentation ind <$> getSettings f
        readIORef aborted >>= \case
          Abort -> pure ()
          Continue -> do
            hPutStrLn stderr $ "Pretty printing " ++ f
            lua <- doReadFile f
            for_ (prettyPrint lintsettings lua) $ \result ->
                doWriteFile f result

overrideLintSettingsIndentation :: Maybe Indentation -> (LintSettings -> LintSettings)
overrideLintSettingsIndentation ind settings = settings
  { prettyprint_indentation = maybe (prettyprint_indentation settings) unIndentation ind
  }

-- | Lint a set of files, uses parsec's parser library
-- Bool return value indicates whether there were either warnings or errors
legacyLint :: Maybe LintSettings -> [FilePath] -> IO Bool
legacyLint _ [] = pure False
legacyLint ls (f : fs) = do
    settings <- getSettings f
    let config = fromJust $ ls <|> Just settings
    logFormat <- logFormatChoiceToLogFormat $ log_format config

    -- When we're dealing with a directory, lint all the files in it recursively.
    isDirectory <- doesDirectoryExist f

    hasMsgs <- if isDirectory then findLuaFiles (lint_ignoreFiles config) f >>= legacyLint ls
        else if f == "stdin" then do
            msgs <- lint config f <$> getContents

            mapM_ (putStrLn . formatLintMessage logFormat) msgs
            pure $ not $ null msgs
        else do
            msgs <- lint config f <$> doReadFile f
            mapM_ (putStrLn . formatLintMessage logFormat) msgs
            pure $ not $ null msgs

    -- Lint the other files
    (|| hasMsgs) <$> legacyLint ls fs

dumpASTStdin :: LintSettings -> String -> IO ()
dumpASTStdin settings = dumpASTFile settings "stdin"

dumpASTFile :: LintSettings -> FilePath -> String -> IO ()
dumpASTFile settings filePath contents =
    case parseFile settings filePath contents of
      Left errs -> mapM_ print errs
      Right (_lexWarnings, ast) ->
        BL.putStr $ JSON.encode ast

dumpAST :: IORef Abort -> [FilePath] -> IO ()
dumpAST aborted fs =
    forEachInput_ Nothing Nothing (UseFiles fs) aborted dumpASTStdin dumpASTFile

runTest :: [FilePath] -> IO ()
runTest fs = do
    putStrLn "Running tests"
    for_ fs $ \f -> do
        isDirectory <- doesDirectoryExist f
        if isDirectory then do
          luaFiles <- findLuaFiles [] f
          runTest luaFiles
        else do
            contents <- doReadFile f
            let (uu_lex, uu_lex_errors) = Lex.execParseTokens contents

            unless (null uu_lex_errors) $ do
                putStrLn $ "Errors when trying to lex '" ++ f ++
                    "' with uu-parsinglib lexer!"
                print uu_lex_errors

            let (uu_ast, uu_parseErrs) = parseGLua $ filter (not . isWhitespace) uu_lex
            lintsettings <- getSettings f
            putStrLn $ "Testing " ++ f
            unless (null uu_parseErrs) $ do
                putStrLn $ "Errors when trying to parse '" ++ f ++
                    "' with uu-parsinglib parser!"
                print uu_parseErrs

            case parseFile lintsettings f contents of
                Right _ -> pure ()
                Left errs -> do
                    putStrLn $ "Errors when trying to parse '" ++ f ++ "' with parsec parser!"
                    print errs

            let pretty_printed = prettyprintConf (lint2ppSetting lintsettings) uu_ast
            let (_uu_ast_pp, uu_parseErrs_pp) = parseGLuaFromString pretty_printed
            unless (null uu_parseErrs_pp) $ do
                putStrLn $ "Errors when trying to parse '" ++ f ++
                    "' with uu-parsinglib parser after pretty print!"
                print uu_parseErrs

            case parseFile lintsettings f pretty_printed of
                Right _ -> pure ()
                Left errs -> do
                    putStrLn $ "Errors when trying to parse '" ++ f ++
                        "' with parsec parser after pretty print!"
                    print errs


--
-- Legacy
--

-- | Deprecated and naive command line interface, kept in place for backwards
-- compatibility. Returns Nothing when this parser fails as well.
legacyCli
    :: IORef Abort
    -> Maybe Indentation
    -> [String]
    -> IO (Maybe (Maybe LintSettings, [FilePath]))
legacyCli aborted ind = \case
  ["--config"] -> pure Nothing

  -- fail when a subcommand of the new parser is given as argument
  "lint" : _ -> pure Nothing
  "pretty-print" : _ -> pure Nothing
  "analyse-globals" : _ -> pure Nothing
  "dump-ast" : _ -> pure Nothing
  "test" : _ -> pure Nothing
  "version" : _ -> pure Nothing

  -- End of recursion case
  [] -> pure $ Just (Nothing, [])
  "--pretty-print-files" : fs -> legacyPrettyPrintFiles aborted ind fs >> exitSuccess
  "--pretty-print" : _ -> legacyPrettyPrintStdin ind >> exitSuccess
  "--analyse-globals" : fs -> analyseGlobals fs >> exitSuccess
  "--dump-ast" : fs -> dumpAST aborted fs >> exitSuccess
  "--version" : _ -> putStrLn version >> exitSuccess
  "--test" : fs -> runTest fs >> exitSuccess
  "--stdin" : xs -> do
    legacyCli aborted ind xs <&> \case
      Nothing -> Nothing
      Just (sets, pths) -> Just (sets, "stdin" : pths)
  "--config" : f : xs -> do
    settings <- settingsFromFile f
    legacyCli aborted ind xs <&> \case
      Nothing -> Nothing
      Just (_, fps) -> Just (settings, fps)
  ('-' : '-' : 'i' : 'n' : 'd' : 'e' : 'n' : 't' : 'a' : 't' : 'i' : 'o' : 'n' : '=' : '\'' : ind') : xs ->
    legacyCli aborted (Just $ Indentation $ init ind') xs
  ('-' : '-' : 'i' : 'n' : 'd' : 'e' : 'n' : 't' : 'a' : 't' : 'i' : 'o' : 'n' : '=' : ind') : xs ->
    legacyCli aborted (Just $ Indentation ind') xs
  f : xs -> do
    legacyCli aborted ind xs <&> \case
      Nothing -> Nothing
      Just (ls, fs) -> Just (ls, f : fs)