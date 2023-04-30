{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GLuaFixer.Effects.Files where

import Control.DeepSeq (deepseq, force)
import Control.Monad.IO.Class (liftIO)
import Data.List (foldl', stripPrefix)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Dispatch.Static (HasCallStack)
import qualified System.Directory as Dir
import System.FilePath (takeDirectory, (</>))
import System.FilePath.Find (FindClause, always, fileName, filePath, find, (&&?), (/~?), (~~?))
import System.FilePath.GlobPattern (GlobPattern)
import System.IO (IOMode (..), hGetContents, hPutStrLn, hSetEncoding, utf8_bom, withFile)
import Prelude hiding (readFile, writeFile)

newtype Directory = Directory {dir :: FilePath}
newtype FileNames = FileNames {names :: [String]}
newtype IgnoreFiles = IgnoreFiles {ignore :: [GlobPattern]}

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

makeAbsolute :: (Files :> es) => FilePath -> Eff es FilePath
makeAbsolute filepath = send $ MakeAbsolute filepath

getHomeDirectory :: Files :> es => Eff es FilePath
getHomeDirectory = send GetHomeDirectory

findLuaFiles :: Files :> es => IgnoreFiles -> FilePath -> Eff es [FilePath]
findLuaFiles ignoreFiles filepath
  = send $ FindLuaFiles ignoreFiles filepath

firstExists ::
  forall (es_a9mjb :: [Effect]). (HasCallStack, Files :> es_a9mjb) =>
                                 [FilePath] -> Eff es_a9mjb (Maybe FilePath)
firstExists x_a9mjc = send ((FirstExists @(Eff es_a9mjb)) x_a9mjc)

searchUpwardsForFile ::
  forall (es_a9mj8 :: [Effect]). (HasCallStack, Files :> es_a9mj8) =>
                                 Directory -> FileNames -> Eff es_a9mj8 (Maybe FilePath)
searchUpwardsForFile x_a9mj9 x_a9mja
  = send ((SearchUpwardsForFile @(Eff es_a9mj8)) x_a9mj9 x_a9mja)

isDirectory ::
  forall (es_a9mj6 :: [Effect]). (HasCallStack, Files :> es_a9mj6) =>
                                 FilePath -> Eff es_a9mj6 Bool
isDirectory x_a9mj7 = send ((IsDirectory @(Eff es_a9mj6)) x_a9mj7)

fileExists ::
  forall (es_a9mj4 :: [Effect]). (HasCallStack, Files :> es_a9mj4) =>
                                 FilePath -> Eff es_a9mj4 Bool
fileExists x_a9mj5 = send ((FileExists @(Eff es_a9mj4)) x_a9mj5)

writeFile ::
  forall (es_a9mj1 :: [Effect]). (HasCallStack, Files :> es_a9mj1) =>
                                 FilePath -> String -> Eff es_a9mj1 ()
writeFile x_a9mj2 x_a9mj3
  = send ((WriteFile @(Eff es_a9mj1)) x_a9mj2 x_a9mj3)

readFile ::
  forall (es_a9miZ :: [Effect]). (HasCallStack, Files :> es_a9miZ) =>
                                 FilePath -> Eff es_a9miZ String
readFile x_a9mj0 = send ((ReadFile @(Eff es_a9miZ)) x_a9mj0)

readStdIn ::
  forall (es_a9miY :: [Effect]). (HasCallStack, Files :> es_a9miY) =>
                                 Eff es_a9miY String
readStdIn = send (ReadStdIn @(Eff es_a9miY))

getCurrentDirectory ::
  forall (es_a9miX :: [Effect]). (HasCallStack, Files :> es_a9miX) =>
                                 Eff es_a9miX FilePath
getCurrentDirectory = send (GetCurrentDirectory @(Eff es_a9miX))

runFilesIO :: forall es a. (IOE :> es) => Eff (Files : es) a -> Eff es a
runFilesIO = interpret $ \_ -> \case
  GetCurrentDirectory -> liftIO Dir.getCurrentDirectory
  ReadStdIn -> liftIO getContents
  ReadFile file -> liftIO $ do
    withFile file ReadMode $ \handle -> do
      hSetEncoding handle utf8_bom
      !contents <- force <$> hGetContents handle
      pure contents
  WriteFile file contents -> deepseq contents $
    liftIO $
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
            if not dirExists
              then pure Nothing
              else go up
     in
      go directory
  FirstExists filepath -> case filepath of
    (file : files) -> do
      exists <- runFilesIO $ fileExists file
      if exists
        then pure $ Just file
        else runFilesIO $ firstExists files
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
