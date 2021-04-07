{-# LANGUAGE LambdaCase #-}

module Main where

import "glualint-lib" GLua.AG.PrettyPrint
import "glualint-lib" GLua.Parser
import "glualint-lib" GLua.ASTInstances ()
import "glualint-lib" GLua.TokenTypes (isWhitespace)
import "glualint-lib" GLuaFixer.AG.ASTLint
import "glualint-lib" GLuaFixer.AG.DarkRPRewrite
import "glualint-lib" GLuaFixer.AnalyseProject
import "glualint-lib" GLuaFixer.LintMessage
import "glualint-lib" GLuaFixer.LintSettings
import "glualint-lib" GLuaFixer.Util

import qualified "glualint-lib" GLua.Lexer as Lex

import Control.Applicative ((<|>), optional)
import Control.Monad (unless, void)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, fromJust)
import Options.Applicative ((<**>))
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure, ExitCode (..))
import System.IO (hPutStrLn, stderr)
import Data.Foldable

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BL
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help.Types as Opt


version :: String
version = "1.17.3"

main :: IO ()
main = do
  args <- getArgs
  let prefs = Opt.defaultPrefs { Opt.prefShowHelpOnEmpty = True }
  let cliParseResult = Opt.execParserPure prefs cliParserInfo args

  case cliParseResult of
    Opt.Success options -> runGluaLint options
    Opt.CompletionInvoked _completionResult -> void $ Opt.handleParseResult cliParseResult
    Opt.Failure parserFailure -> do
      progName <- getProgName
      handleCliFailure args $ Opt.execFailure parserFailure progName

-- | Command line options of glualint
data Options
   = Options
     { optsConfigFile :: Maybe FilePath
     , optsIndentation :: Maybe Indentation
     , optsOutputFormat :: Maybe LogFormatChoice
     , optsCommand :: Command
     } deriving (Show)

-- | Available subcommands
data Command
   = Lint StdInOrFiles
   | PrettyPrint StdInOrFiles
   | AnalyseGlobals [FilePath]
   | DumpAst StdInOrFiles
   | Test [FilePath]
   | PrintVersion
   deriving (Show)

-- | Metadata of the application
cliParserInfo :: Opt.ParserInfo Options
cliParserInfo = Opt.info (cliParser <**> Opt.helper)
  (  Opt.fullDesc
  <> Opt.progDesc "Linter and pretty printer for Garry's mod's flavour of Lua."
  <> Opt.header "glualint - lint and pretty print GLua files."
  )

-- | Defines the command line interface parser
cliParser :: Opt.Parser Options
cliParser = Options
    <$> configOption
    <*> indentOption
    <*> outputFormatOption
    <*> commandParser
  where
    configOption :: Opt.Parser (Maybe FilePath)
    configOption =
      optional $ Opt.strOption
        (  Opt.long "config"
        <> Opt.metavar "PATH"
        <> Opt.help "Explicitly define config file location. By default it will search for it."
        )

    indentOption :: Opt.Parser (Maybe String)
    indentOption =
      optional $ Opt.strOption
        (  Opt.long "indentation"
        <> Opt.metavar "STR"
        <> Opt.help "What to use for indentation when pretty printing, 4 spaces by default."
        )

    outputFormatOption :: Opt.Parser (Maybe LogFormatChoice)
    outputFormatOption =
      optional $ Opt.option outputFormatReader
        (  Opt.long "output-format"
        <> Opt.metavar "FORMAT"
        <> Opt.help "Logging format, either 'auto', 'standard' or 'github', defaults to 'standard'"
        )

    outputFormatReader :: Opt.ReadM LogFormatChoice
    outputFormatReader = Opt.eitherReader $ \case
      "standard" -> Right $ LogFormatChoice StandardLogFormat
      "github" -> Right $ LogFormatChoice GithubLogFormat
      "auto" -> Right AutoLogFormatChoice
      val -> Left $ "Bad output format '" <> val <> "', must be either 'auto', 'standard' or 'github'."

    commandParser :: Opt.Parser Command
    commandParser = Opt.hsubparser
      (  Opt.command
           "lint"
           (Opt.info (Lint <$> parseStdInOrFiles) $
           Opt.progDesc "Lint the given files. Directories will be traversed recursively.")
      <> Opt.command
           "pretty-print"
           (Opt.info (PrettyPrint <$> parseStdInOrFiles) $
           Opt.progDesc "Pretty print the given files, replacing their contents with the pretty printed code.")
      <> Opt.command
           "analyse-globals"
           (Opt.info (AnalyseGlobals <$> filesArgument) $
           Opt.progDesc "Print a list of all globals used and defined in the given files/directories.")
      <> Opt.command
           "dump-ast"
           (Opt.info (DumpAst <$> parseStdInOrFiles) $
           Opt.progDesc "Print a list of all globals used and defined in the given files/directories.")
      <> Opt.command
           "test"
           (Opt.info (Test <$> filesArgument) $
           Opt.progDesc "Run tests on the given files. Use for testing/debugging glualint.")
      <> Opt.command
           "version"
           (Opt.info (pure PrintVersion) $
           Opt.progDesc "Print the version of glualint and exit.")
      )

    parseStdInOrFiles :: Opt.Parser StdInOrFiles
    parseStdInOrFiles =
        Opt.flag' UseStdIn
        (  Opt.long "stdin"
        <> Opt.help "Use stdin instead of files."
        )
      <|> UseFiles <$> filesArgument

    filesArgument :: Opt.Parser [FilePath]
    filesArgument = Opt.some (Opt.argument Opt.str $ Opt.metavar "FILES")

-- | Run subcommands
runGluaLint :: Options -> IO ()
runGluaLint opts = do
    case optsCommand opts of
      Lint stdinOrFiles -> do
        noErrorsOrWarnings <- and <$> forEachInput mbIndent mbOutputFormat stdinOrFiles
          lintStdin
          lintFile

        unless noErrorsOrWarnings exitFailure
      PrettyPrint stdinOrFiles -> do
        noErrors <- and <$> forEachInput mbIndent mbOutputFormat stdinOrFiles
          prettyPrintStdin
          prettyPrintFile

        unless noErrors $ exitFailure
      AnalyseGlobals files ->
        analyseGlobals files
      DumpAst stdinOrFiles ->
        forEachInput_ mbIndent mbOutputFormat stdinOrFiles
          dumpASTStdin
          dumpASTFile
      Test files ->
        runTest files
      PrintVersion ->
        putStrLn version
  where
    mbIndent = optsIndentation opts
    mbOutputFormat = optsOutputFormat opts

-- | When the regulare CLI fails to parse, the legacy one might yet succeed
handleCliFailure :: [String] -> (Opt.ParserHelp, ExitCode, Int) -> IO ()
handleCliFailure args (parserHelp, exitCode, terminalColumns) = case exitCode of
  -- This means the help was activated. Print the help
  ExitSuccess -> putStrLn $ Opt.renderHelp terminalColumns parserHelp
  ExitFailure _ -> do
    -- Attempt legacy CLI interface
    mbRes <- legacyCli Nothing args
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
    else Just $ prettyprintConf ppconf $ fixOldDarkRPSyntax ast
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
    mapM_ (putStrLn . formatLintMessage logFormat) msgs
    pure $ null msgs

-- | Lint a string, using parsec
lint :: LintSettings -> FilePath -> String -> [LintMessage]
lint config f contents =
    case parseFile config f contents of
      Left errs -> errs
      Right (lexWarnings, ast) ->
        let
            parserWarnings = map ($f) $ astWarnings config ast
        in
            -- Print all warnings
            sortLintMessages $ lexWarnings ++ parserWarnings

-- | Pretty print, uses the uu-parsinglib library
legacyPrettyPrintStdin :: Maybe Indentation -> IO ()
legacyPrettyPrintStdin ind = do
    lua <- getContents

    cwd <- getCurrentDirectory
    lintsettings <- overrideLintSettingsIndentation ind <$> getSettings cwd

    for_ (prettyPrint lintsettings lua) $ \result ->
        putStr result

legacyPrettyPrintFiles :: Maybe Indentation -> [FilePath] -> IO ()
legacyPrettyPrintFiles ind = mapM_ pp
  where
    pp :: FilePath -> IO ()
    pp f = do
      isDirectory <- doesDirectoryExist f
      if isDirectory then do
        luaFiles <- findLuaFiles [] f
        legacyPrettyPrintFiles ind luaFiles
      else do
        hPutStrLn stderr $ "Pretty printing " ++ f
        lintsettings <- overrideLintSettingsIndentation ind <$> getSettings f
        lua <- doReadFile f
        for_ (prettyPrint lintsettings lua) $ \result ->
            doWriteFile f result

overrideLintSettingsIndentation :: Maybe Indentation -> (LintSettings -> LintSettings)
overrideLintSettingsIndentation ind settings = settings
  { prettyprint_indentation = fromMaybe (prettyprint_indentation settings) ind
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
dumpASTStdin settings contents =
    dumpASTFile settings "stdin" contents

dumpASTFile :: LintSettings -> FilePath -> String -> IO ()
dumpASTFile settings filePath contents =
    case parseFile settings filePath contents of
      Left errs -> mapM_ print errs
      Right (_lexWarnings, ast) ->
        BL.putStr $ JSON.encode ast

dumpAST :: [FilePath] -> IO ()
dumpAST fs =
    forEachInput_ Nothing Nothing (UseFiles fs) dumpASTStdin dumpASTFile

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

            let pretty_printed = prettyprintConf (lint2ppSetting lintsettings) $ fixOldDarkRPSyntax uu_ast
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
legacyCli :: Maybe Indentation -> [String] -> IO (Maybe (Maybe LintSettings, [FilePath]))
legacyCli ind = \case
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
  "--pretty-print-files" : fs -> legacyPrettyPrintFiles ind fs >> exitSuccess
  "--pretty-print" : _ -> legacyPrettyPrintStdin ind >> exitSuccess
  "--analyse-globals" : fs -> analyseGlobals fs >> exitSuccess
  "--dump-ast" : fs -> dumpAST fs >> exitSuccess
  "--version" : _ -> putStrLn version >> exitSuccess
  "--test" : fs -> runTest fs >> exitSuccess
  "--stdin" : xs -> do
    legacyCli ind xs <&> \case
      Nothing -> Nothing
      Just (sets, pths) -> Just (sets, "stdin" : pths)
  "--config" : f : xs -> do
    settings <- settingsFromFile f
    legacyCli ind xs <&> \case
      Nothing -> Nothing
      Just (_, fps) -> Just (settings, fps)
  ('-' : '-' : 'i' : 'n' : 'd' : 'e' : 'n' : 't' : 'a' : 't' : 'i' : 'o' : 'n' : '=' : '\'' : ind') : xs ->
    legacyCli (Just (init ind')) xs
  ('-' : '-' : 'i' : 'n' : 'd' : 'e' : 'n' : 't' : 'a' : 't' : 'i' : 'o' : 'n' : '=' : ind') : xs ->
    legacyCli (Just ind') xs
  f : xs -> do
    legacyCli ind xs <&> \case
      Nothing -> Nothing
      Just (ls, fs) -> Just (ls, f : fs)
