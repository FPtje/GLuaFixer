{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GLuaFixer.Effects.Cli where

import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects (WithSideEffects), StaticRep, evalStaticRep, unsafeEff_)
import qualified Effectful.Environment as Eff
import GHC.IO.Exception (ExitCode (..))
import GLuaFixer.Cli (Options, legacyCliParser, runParse)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help.Types as Opt

-- | Effect for allowing graceful interruptions. Interruptions are polled, so they can be ignored.
data Cli :: Effect

type instance DispatchOf Cli = Static WithSideEffects

data instance StaticRep Cli = Cli

-- | Run the Cli effect
runCliIO :: IOE :> es => Eff (Cli : es) a -> Eff es a
runCliIO = evalStaticRep Cli

{- | The result of parsing the CLI arguments. Either successful, with Options describing the running
parameters of glualint, or alternatively, show the help text and exit with the exit code
-}
data CliParseResult
  = ParseSuccessful Options
  | PrintHelpText ExitCode String

-- | Parse the CLI options
parseCliOptions :: (Eff.Environment :> es, Cli :> es) => Eff es CliParseResult
parseCliOptions = do
  args <- Eff.getArgs

  case runParse args of
    Opt.Success options -> pure $ ParseSuccessful options
    Opt.CompletionInvoked completionResult -> do
      progName <- Eff.getProgName
      helpText <- execCompletion completionResult progName
      pure $ PrintHelpText ExitSuccess helpText
    Opt.Failure parserFailure -> do
      progName <- Eff.getProgName
      let
        (parserHelp, exitCode, terminalColumns) = Opt.execFailure parserFailure progName

      let printHelpText = PrintHelpText exitCode $ Opt.renderHelp terminalColumns parserHelp
      case exitCode of
        -- This means the help was activated. Print the help
        ExitSuccess ->
          pure printHelpText
        ExitFailure _ ->
          -- Attempt legacy CLI interface
          case legacyCliParser args of
            Just options -> pure $ ParseSuccessful options
            Nothing -> pure printHelpText

-- | Calculate the autocomplete string
execCompletion :: Cli :> es => Opt.CompletionResult -> String -> Eff es String
execCompletion cmpletionResult progName =
  unsafeEff_ $ Opt.execCompletion cmpletionResult progName
