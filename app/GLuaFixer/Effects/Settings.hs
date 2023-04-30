{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GLuaFixer.Effects.Settings where

import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import qualified Effectful.Error.Static as Err
import Effectful.TH (makeEffect)
import GLuaFixer.Cli (OverriddenSettings, SettingsPath (..), overrideSettings)
import GLuaFixer.Effects.Files (Files)
import qualified GLuaFixer.Effects.Files as Files
import GLuaFixer.LintSettings (LintSettings, defaultLintSettings)
import System.FilePath (takeDirectory, (</>))

data Settings :: Effect where
  -- | Search for settings in various locations, returns default settings if nothing is found
  GetSettings :: FilePath -> Settings m LintSettings
  -- | Read settings from a glualint.json file
  SettingsFromFile :: FilePath -> Settings m LintSettings
  -- | Search for settings in a directory, going up until it reaches the root or finds the file
  SearchSettingsInDirectory :: FilePath -> Settings m (Maybe FilePath)
  -- | Search settings in the home directory
  SearchSettingsInHome :: Settings m (Maybe FilePath)

type instance DispatchOf Settings = Dynamic

makeEffect ''Settings

newtype SettingsError = CouldNotParseSettings String

-- | The default settings file
settingsFile :: FilePath
settingsFile = "glualint.json"

-- | The default settings file when placed in the home folder
homeSettingsFile :: FilePath
homeSettingsFile = ".glualint.json"

-- | Run settings based on the Files effect.
runSettings :: (Files :> es, Err.Error SettingsError :> es) => Eff (Settings : es) a -> Eff es a
runSettings = interpret $ \_ -> \case
  GetSettings filepath -> do
    absolutePath <-
      if filepath == "stdin"
        then Files.getCurrentDirectory
        else Files.makeAbsolute filepath

    fromSearching <- runSettings $ searchSettingsInDirectory absolutePath
    fromHome <- runSettings searchSettingsInHome

    case fromSearching <|> fromHome of
      Just foundSettings -> runSettings $ settingsFromFile foundSettings
      Nothing -> pure defaultLintSettings
  SettingsFromFile filepath -> do
    contents <- Files.readFile filepath
    let
      eDecoded = Aeson.eitherDecodeStrict @LintSettings $ BS8.pack contents

    case eDecoded of
      Left err -> Err.throwError $ CouldNotParseSettings err
      Right decoded -> pure decoded
  SearchSettingsInDirectory filepath -> do
    isDirectory <- Files.isDirectory filepath
    let
      dir = Files.Directory $ if isDirectory then filepath else takeDirectory filepath
      settingsFiles = Files.FileNames [settingsFile, homeSettingsFile]
    Files.searchUpwardsForFile dir settingsFiles
  SearchSettingsInHome -> do
    homeDir <- Files.getHomeDirectory
    let
      fileWithDot = homeDir </> homeSettingsFile
      fileWithoutDot = homeDir </> settingsFile

    Files.firstExists [fileWithDot, fileWithoutDot]

-- | Combines getting the settings from
-- - An explicitly passed path to a file
-- - Settings overridden in the CLI
-- - Settings that apply to a file
getSettingsForFile ::
  Settings :> es =>
  Maybe SettingsPath ->
  OverriddenSettings ->
  FilePath ->
  Eff es LintSettings
getSettingsForFile mbSettingsPath overridden filepath = do
  readSettings <- case mbSettingsPath of
    Just (SettingsPath path) -> settingsFromFile path
    Nothing -> getSettings filepath
  pure $ overrideSettings overridden readSettings
