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
import GLuaFixer.Cli (Options, runParse)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help.Types as Opt

-- | Effect for allowing graceful interruptions. Interruptions are polled, so they can be ignored.
data Cli :: Effect

type instance DispatchOf Cli = Static WithSideEffects

data instance StaticRep Cli = Cli

-- | Run the Cli effect
runCliIO :: IOE :> es => Eff (Cli : es) a -> Eff es a
runCliIO = evalStaticRep Cli

-- | The result of parsing the CLI arguments. Either successful, with Options describing the running
-- parameters of glualint, or failure, where a help text is shown.
data CliParseResult
  = ParseSuccessful Options
  | PrintHelpText String

-- | Parse the CLI options
parseCliOptions :: (Eff.Environment :> es, Cli :> es) => Eff es CliParseResult
parseCliOptions = do
  args <- Eff.getArgs

  case runParse args of
    Opt.Success options -> pure $ ParseSuccessful options
    Opt.CompletionInvoked completionResult -> do
      progName <- Eff.getProgName
      helpText <- execCompletion completionResult progName
      pure $ PrintHelpText helpText
    Opt.Failure parserFailure -> do
      progName <- Eff.getProgName
      let
        (parserHelp, _exitCode, terminalColumns) = Opt.execFailure parserFailure progName

      pure $ PrintHelpText $ Opt.renderHelp terminalColumns parserHelp

-- | Calculate the autocomplete string
execCompletion :: Cli :> es => Opt.CompletionResult -> String -> Eff es String
execCompletion cmpletionResult progName =
  unsafeEff_ $ Opt.execCompletion cmpletionResult progName
