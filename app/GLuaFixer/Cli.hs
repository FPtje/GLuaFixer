{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | CLI interface of the glualint executable
module GLuaFixer.Cli (SettingsPath (..), Options (..), Command (..), OverriddenSettings (..), overrideSettings, runParse, legacyCliParser) where

import Control.Applicative ((<**>), (<|>))
import Data.Maybe (fromMaybe)
import GLuaFixer.LintMessage (LogFormat (..), LogFormatChoice (..))
import GLuaFixer.LintSettings (Indentation (..), LintSettings (..), StdInOrFiles (..))
import Options.Applicative (optional)
import qualified Options.Applicative as Opt

newtype SettingsPath = SettingsPath FilePath
  deriving newtype (Show)

-- | Command line options of glualint
data Options = Options
  { optsConfigFile :: Maybe SettingsPath
  , optsOverridden :: OverriddenSettings
  , optsCommand :: Command
  , optsFiles :: StdInOrFiles
  , optsDebug :: Bool
  }
  deriving (Show)

-- | Available subcommands
data Command
  = Lint
  | PrettyPrint
  | AnalyseGlobals
  | DumpLexicon
  | DumpAst
  | Test
  | PrintVersion
  deriving (Show)

-- | Settings in the config file that can be overridden through the command line.
data OverriddenSettings = OverriddenSettings
  { indentation :: Maybe Indentation
  , outputFormat :: Maybe LogFormatChoice
  }
  deriving (Show)

-- | Override settings with the options passed on the command line
overrideSettings :: OverriddenSettings -> LintSettings -> LintSettings
overrideSettings overridden settings =
  settings
    { prettyprint_indentation =
        maybe settings.prettyprint_indentation unIndentation overridden.indentation
    , log_format = fromMaybe settings.log_format overridden.outputFormat
    }

-- | Run the parser against arguments
runParse :: [String] -> Opt.ParserResult Options
runParse = Opt.execParserPure cliPreferences cliParserInfo

-- | Preferences passed to optparse-applicative
cliPreferences :: Opt.ParserPrefs
cliPreferences = Opt.defaultPrefs{Opt.prefShowHelpOnEmpty = True}

-- | Metadata of the application
cliParserInfo :: Opt.ParserInfo Options
cliParserInfo =
  Opt.info
    (cliParser <**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.progDesc "Linter and pretty printer for Garry's mod's flavour of Lua."
        <> Opt.header "glualint - lint and pretty print GLua files."
    )

-- | Defines the command line interface parser
cliParser :: Opt.Parser Options
cliParser =
  Options
    <$> configOption
    <*> (OverriddenSettings <$> indentOption <*> outputFormatOption)
    <*> commandParser
    <*> parseStdInOrFiles
    <*> parseDebug
  where
    configOption :: Opt.Parser (Maybe SettingsPath)
    configOption =
      optional $
        SettingsPath
          <$> Opt.strOption
            ( Opt.long "config"
                <> Opt.metavar "PATH"
                <> Opt.help "Explicitly define config file location. By default it will search for it."
            )

    indentOption :: Opt.Parser (Maybe Indentation)
    indentOption =
      optional $
        Opt.strOption
          ( Opt.long "indentation"
              <> Opt.metavar "STR"
              <> Opt.help "What to use for indentation when pretty printing, 4 spaces by default."
          )

    outputFormatOption :: Opt.Parser (Maybe LogFormatChoice)
    outputFormatOption =
      optional $
        Opt.option
          outputFormatReader
          ( Opt.long "output-format"
              <> Opt.metavar "FORMAT"
              <> Opt.help "Logging format, either 'auto', 'standard' or 'github', defaults to 'standard'"
          )

    outputFormatReader :: Opt.ReadM LogFormatChoice
    outputFormatReader = Opt.eitherReader $ \case
      "standard" -> Right $ LogFormatChoice StandardLogFormat
      "github" -> Right $ LogFormatChoice GithubLogFormat
      "auto" -> Right AutoLogFormatChoice
      val -> Left $ "Bad output format '" <> val <> "', must be either 'auto', 'standard' or 'github'."

    parseDebug :: Opt.Parser Bool
    parseDebug =
      Opt.switch
        ( Opt.long "debug"
            <> Opt.help "Whether to enable some debug prints."
        )

    commandParser :: Opt.Parser Command
    commandParser =
      Opt.hsubparser
        ( Opt.command
            "lint"
            ( Opt.info (pure Lint) $
                Opt.progDesc "Lint the given files. Directories will be traversed recursively."
            )
            <> Opt.command
              "pretty-print"
              ( Opt.info (pure PrettyPrint) $
                  Opt.progDesc "Pretty print the given files, replacing their contents with the pretty printed code."
              )
            <> Opt.command
              "analyse-globals"
              ( Opt.info (pure AnalyseGlobals) $
                  Opt.progDesc "Print a list of all globals used and defined in the given files/directories."
              )
            <> Opt.command
              "dump-lexicon"
              ( Opt.info (pure DumpLexicon) $
                  Opt.progDesc "Debug command to dump the tokens parsed in the Lua code."
              )
            <> Opt.command
              "dump-ast"
              ( Opt.info (pure DumpAst) $
                  Opt.progDesc "Debug command to dump the internal Abstract Syntax Tree of the Lua code."
              )
            <> Opt.command
              "test"
              ( Opt.info (pure Test) $
                  Opt.progDesc "Run tests on the given files. Use for testing/debugging glualint."
              )
            <> Opt.command
              "version"
              ( Opt.info (pure PrintVersion) $
                  Opt.progDesc "Print the version of glualint and exit."
              )
        )

    parseStdInOrFiles :: Opt.Parser StdInOrFiles
    parseStdInOrFiles =
      Opt.flag'
        UseStdIn
        ( Opt.long "stdin"
            <> Opt.help "Use stdin instead of files."
        )
        <|> UseFiles <$> filesArgument

    filesArgument :: Opt.Parser [FilePath]
    filesArgument = Opt.many (Opt.argument Opt.str $ Opt.metavar "FILES")

--
-- Legacy
--

-- | Deprecated and naive command line interface, kept in place for backwards
-- compatibility.
legacyCliParser :: [String] -> Maybe Options
legacyCliParser args = case go False emptyOptions args of
  (False, _) -> Nothing
  (True, opts@Options{optsCommand = Lint, optsFiles = files}) -> case files of
    UseFiles [] -> Nothing
    _ -> Just opts
  (True, opts) -> Just opts
  where
    emptyOptions =
      Options
        { optsConfigFile = Nothing
        , optsOverridden = OverriddenSettings Nothing Nothing
        , optsCommand = Lint
        , optsFiles = UseFiles []
        , optsDebug = False
        }

    go hasSuccessfulParse options = \case
      ["--config"] -> (False, options)
      -- fail when a subcommand of the new parser is given as argument
      "lint" : _ -> (False, options)
      "pretty-print" : _ -> (False, options)
      "analyse-globals" : _ -> (False, options)
      "dump-ast" : _ -> (False, options)
      "test" : _ -> (False, options)
      "version" : _ -> (False, options)
      "--bash-completion-script" : _ -> (False, options)
      "--fish-completion-script" : _ -> (False, options)
      "--zsh-completion-script" : _ -> (False, options)
      -- End of recursion case
      [] -> (hasSuccessfulParse, options)
      "--debug" : xs -> go True options{optsDebug = True} xs
      "--pretty-print-files" : xs -> go True options{optsCommand = PrettyPrint} xs
      "--pretty-print" : xs -> go True options{optsCommand = PrettyPrint, optsFiles = UseStdIn} xs
      "--analyse-globals" : xs -> go True options{optsCommand = AnalyseGlobals} xs
      "--dump-ast" : xs -> go True options{optsCommand = DumpAst} xs
      "--version" : xs -> go True options{optsCommand = PrintVersion} xs
      "--test" : xs -> go True options{optsCommand = Test} xs
      "--stdin" : xs -> go True options{optsFiles = UseStdIn} xs
      "--config" : f : xs -> go True options{optsConfigFile = Just $ SettingsPath f} xs
      ('-' : '-' : 'i' : 'n' : 'd' : 'e' : 'n' : 't' : 'a' : 't' : 'i' : 'o' : 'n' : '=' : '\'' : ind') : xs ->
        go True options{optsOverridden = options.optsOverridden{indentation = Just $ Indentation ind'}} xs
      ('-' : '-' : 'i' : 'n' : 'd' : 'e' : 'n' : 't' : 'a' : 't' : 'i' : 'o' : 'n' : '=' : ind') : xs ->
        go True options{optsOverridden = options.optsOverridden{indentation = Just $ Indentation ind'}} xs
      f : xs -> case optsFiles options of
        UseStdIn -> go True options{optsFiles = UseFiles [f]} xs
        UseFiles fs -> go True options{optsFiles = UseFiles $ f : fs} xs
