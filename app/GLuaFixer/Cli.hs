{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | CLI interface of the glualint executable
module GLuaFixer.Cli (Options (..), Command (..), OverriddenSettings (..), overrideSettings, runParse, legacyCliParser) where

import Control.Applicative ((<**>), (<|>))
import Data.Maybe (fromMaybe)
import GLuaFixer.LintMessage (LogFormat (..), LogFormatChoice (..))
import GLuaFixer.LintSettings (Indentation (..), LintSettings (..), StdInOrFiles (..))
import Options.Applicative (optional)
import qualified Options.Applicative as Opt

newtype SettingsPath = SettingsPath FilePath

-- | Command line options of glualint
data Options = Options
  { optsConfigFile :: Maybe SettingsPath
  , optsOverridden :: OverriddenSettings
  , optsCommand :: Command
  , optsFiles :: StdInOrFiles
  }

-- | Available subcommands
data Command
  = Lint
  | PrettyPrint
  | AnalyseGlobals
  | DumpAst
  | Test
  | PrintVersion
  deriving (Show)

-- | Settings in the config file that can be overridden through the command line.
data OverriddenSettings = OverriddenSettings
  { indentation :: Maybe Indentation
  , outputFormat :: Maybe LogFormatChoice
  }

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
            "dump-ast"
            ( Opt.info (pure DumpAst) $
                Opt.progDesc "Print a list of all globals used and defined in the given files/directories."
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

{- | Deprecated and naive command line interface, kept in place for backwards
compatibility.
-}
legacyCliParser :: [String] -> Maybe Options
legacyCliParser = go emptyOptions
 where
  emptyOptions =
    Options
      { optsConfigFile = Nothing
      , optsOverridden = OverriddenSettings Nothing Nothing
      , optsCommand = PrintVersion
      , optsFiles = UseFiles []
      }

  go options = \case
    ["--config"] -> Nothing
    -- fail when a subcommand of the new parser is given as argument
    "lint" : _ -> Nothing
    "pretty-print" : _ -> Nothing
    "analyse-globals" : _ -> Nothing
    "dump-ast" : _ -> Nothing
    "test" : _ -> Nothing
    "version" : _ -> Nothing
    -- End of recursion case
    [] -> Just options
    "--pretty-print-files" : xs -> go options{optsCommand = PrettyPrint} xs
    "--pretty-print" : xs -> go options{optsCommand = PrettyPrint} xs
    "--analyse-globals" : xs -> go options{optsCommand = AnalyseGlobals} xs
    "--dump-ast" : xs -> go options{optsCommand = DumpAst} xs
    "--version" : xs -> go options{optsCommand = PrintVersion} xs
    "--test" : xs -> go options{optsCommand = Test} xs
    "--stdin" : xs -> go options{optsFiles = UseStdIn} xs
    "--config" : f : xs -> go options{optsConfigFile = Just $ SettingsPath f} xs
    ('-' : '-' : 'i' : 'n' : 'd' : 'e' : 'n' : 't' : 'a' : 't' : 'i' : 'o' : 'n' : '=' : '\'' : ind') : xs ->
      go options{optsOverridden = options.optsOverridden{indentation = Just $ Indentation ind'}} xs
    ('-' : '-' : 'i' : 'n' : 'd' : 'e' : 'n' : 't' : 'a' : 't' : 'i' : 'o' : 'n' : '=' : ind') : xs ->
      go options{optsOverridden = options.optsOverridden{indentation = Just $ Indentation ind'}} xs
    f : xs -> case optsFiles options of
      UseStdIn -> go options{optsFiles = UseFiles [f]} xs
      UseFiles fs -> go options{optsFiles = UseFiles $ f : fs} xs
