{-# LANGUAGE LambdaCase #-}

-- | CLI interface of the glualint executable
module GLuaFixer.Cli where

import Control.Applicative ((<**>), (<|>))
import GLuaFixer.LintMessage (LogFormat (..), LogFormatChoice (..))
import GLuaFixer.LintSettings (Indentation, StdInOrFiles (..))
import Options.Applicative (optional)
import qualified Options.Applicative as Opt

-- | Command line options of glualint
data Options = Options
  { optsConfigFile :: Maybe FilePath
  , optsIndentation :: Maybe Indentation
  , optsOutputFormat :: Maybe LogFormatChoice
  , optsCommand :: Command
  }

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
    <*> indentOption
    <*> outputFormatOption
    <*> commandParser
 where
  configOption :: Opt.Parser (Maybe FilePath)
  configOption =
    optional $
      Opt.strOption
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
          ( Opt.info (Lint <$> parseStdInOrFiles) $
              Opt.progDesc "Lint the given files. Directories will be traversed recursively."
          )
          <> Opt.command
            "pretty-print"
            ( Opt.info (PrettyPrint <$> parseStdInOrFiles) $
                Opt.progDesc "Pretty print the given files, replacing their contents with the pretty printed code."
            )
          <> Opt.command
            "analyse-globals"
            ( Opt.info (AnalyseGlobals <$> filesArgument) $
                Opt.progDesc "Print a list of all globals used and defined in the given files/directories."
            )
          <> Opt.command
            "dump-ast"
            ( Opt.info (DumpAst <$> parseStdInOrFiles) $
                Opt.progDesc "Print a list of all globals used and defined in the given files/directories."
            )
          <> Opt.command
            "test"
            ( Opt.info (Test <$> filesArgument) $
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
  filesArgument = Opt.some (Opt.argument Opt.str $ Opt.metavar "FILES")
