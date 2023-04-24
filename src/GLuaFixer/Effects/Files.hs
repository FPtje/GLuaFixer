{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module GLuaFixer.Effects.Files where

import Prelude hiding (readFile, writeFile)
import Data.Maybe(fromMaybe, mapMaybe, listToMaybe)
import Effectful ( Effect, Dispatch(Dynamic), DispatchOf, IOE, (:>), Eff )
import qualified System.Directory as Dir
import Effectful.TH ( makeEffect )
import Effectful.Dispatch.Dynamic (interpret)
import Data.List (foldl', stripPrefix)
import Control.Monad.IO.Class (liftIO)
import System.IO (withFile, hSetEncoding, utf8_bom, hGetContents, hPutStrLn, IOMode (..))
import Control.DeepSeq (deepseq, force)
import System.FilePath ((</>), takeDirectory)
import System.FilePath.GlobPattern (GlobPattern)
import System.FilePath.Find (find, always, fileName, filePath, (~~?), (&&?), (/~?), FindClause)

newtype Directory = Directory { dir :: FilePath }
newtype FileNames = FileNames { names :: [String] }
newtype IgnoreFiles = IgnoreFiles { ignore :: [GlobPattern] }

-- | Interacting with the file system
data Files :: Effect where
  -- | Get current working directory alias
  GetCurrentDirectory :: Files m FilePath
  -- | Read stdin
  ReadStdIn :: Files m String
  -- | Read a file
  ReadFile :: FilePath -> Files m String
  -- | Write to a file
  WriteFile :: FilePath -> String -> Files m ()
  -- | Returns whether a file exists
  FileExists :: FilePath -> Files m Bool
  -- | Returns whether a file exists and is a directory
  IsDirectory :: FilePath -> Files m Bool
  -- | Will search upwards in directories for a file, returning it if it exists
  SearchUpwardsForFile :: Directory -> FileNames -> Files m (Maybe FilePath)
  -- | Returns the file path of the first path that exists
  FirstExists :: [FilePath] -> Files m (Maybe FilePath)
  -- | Search for Lua files
  FindLuaFiles :: IgnoreFiles -> FilePath -> Files m [FilePath]
  -- | Get Home directory
  GetHomeDirectory :: Files m FilePath
  -- | Make a path absolute
  MakeAbsolute :: FilePath -> Files m FilePath


type instance DispatchOf Files = Dynamic

makeEffect ''Files

runFilesIO :: forall es a.  (IOE :> es) => Eff (Files : es) a -> Eff es a
runFilesIO = interpret $ \_ -> \case
  GetCurrentDirectory -> liftIO Dir.getCurrentDirectory

  ReadStdIn -> liftIO getContents

  ReadFile file -> liftIO $ do
    withFile file ReadMode $ \handle -> do
      hSetEncoding handle utf8_bom
      !contents <- force <$> hGetContents handle
      pure contents

  WriteFile file contents -> deepseq contents $ liftIO $
    withFile file WriteMode $ \handle -> do
      hSetEncoding handle utf8_bom
      hPutStrLn handle contents

  FileExists filepath -> liftIO $ Dir.doesFileExist filepath

  IsDirectory filepath -> liftIO $ Dir.doesDirectoryExist filepath

  SearchUpwardsForFile (Directory directory) (FileNames filename) ->
    let
      go !subdir = do
        let filepaths = fmap (subdir </>) filename
        mbFoundFile <- runFilesIO $ firstExists filepaths
        case mbFoundFile of
          Just file -> pure $ Just file
          Nothing -> do
            let up = takeDirectory subdir
            dirExists <- runFilesIO $ isDirectory up
            if not dirExists then
              pure Nothing
            else
              go up

    in go directory

  FirstExists filepath -> case filepath of
    (file : files) -> do
      exists <- runFilesIO $ fileExists file
      if exists then
        pure $ Just file
      else
        runFilesIO $ firstExists files
    [] -> pure Nothing

  FindLuaFiles (IgnoreFiles ignoreFiles) path ->
    let
      ignoredGlobs = foldl' (&&?) always $ map (relativeFilePath /~?) ignoreFiles

      relativeFilePath :: FindClause FilePath
      relativeFilePath = fmap stripFromPath filePath

      -- Turn absolute path into relative path by stripping prefix
      stripFromPath :: FilePath -> FilePath
      stripFromPath fp =
        fromMaybe fp $ listToMaybe $ mapMaybe (`stripPrefix` fp) pathsToStrip

      -- Prefixes to attempt to strip, in order
      pathsToStrip =
        [ "./"
        , path ++ "/"
        , path
        ]
    in
      liftIO $ find always (fileName ~~? "*.lua" &&? ignoredGlobs) path

  GetHomeDirectory -> liftIO Dir.getHomeDirectory

  MakeAbsolute filepath -> liftIO $ Dir.makeAbsolute filepath
