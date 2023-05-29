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
import Effectful.Dispatch.Dynamic (interpret, send, interpose)
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

firstExists :: Files :> es => [FilePath] -> Eff es (Maybe FilePath)
firstExists files = send $ FirstExists files

searchUpwardsForFile :: Files :> es => Directory -> FileNames -> Eff es (Maybe FilePath)
searchUpwardsForFile directory files = send $ SearchUpwardsForFile directory files

isDirectory :: Files :> es => FilePath -> Eff es Bool
isDirectory filepath = send $ IsDirectory filepath

fileExists :: Files :> es => FilePath -> Eff es Bool
fileExists filepath = send $ FileExists  filepath

writeFile :: Files :> es => FilePath -> String -> Eff es ()
writeFile filepath contents = send $ WriteFile filepath contents

readFile :: Files :> es => FilePath -> Eff es String
readFile filepath = send $ ReadFile filepath

readStdIn :: Files :> es => Eff es String
readStdIn = send ReadStdIn

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
            if up == subdir || not dirExists
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

-- | Trace actions on files, useful for debugging
traceFiles :: (Files :> es, IOE :> es) => Eff es a -> Eff es a
traceFiles = interpose $ \_ -> \case
  GetCurrentDirectory -> do
    liftIO $ putStrLn "GetCurrentDirectory"
    send GetCurrentDirectory
  ReadStdIn -> do
    liftIO $ putStrLn "ReadStdIn"
    send ReadStdIn
  ReadFile filepath -> do
    liftIO $ putStrLn "ReadFile"
    send $ ReadFile filepath
  WriteFile filepath contents -> do
    liftIO $ putStrLn "WriteFile"
    send $ WriteFile  filepath contents
  FileExists filepath -> do
    liftIO $ putStrLn "FileExists"
    send $ FileExists filepath
  IsDirectory filepath -> do
    liftIO $ putStrLn "IsDirectory"
    send $ IsDirectory filepath
  SearchUpwardsForFile directory filenames -> do
    liftIO $ putStrLn "SearchUpwardsForFile"
    send $ SearchUpwardsForFile directory filenames
  FirstExists filepath -> do
    liftIO $ putStrLn "FirstExists"
    send $ FirstExists filepath
  FindLuaFiles ignores filepath -> do
    liftIO $ putStrLn "FindLuaFiles"
    send $ FindLuaFiles ignores filepath
  GetHomeDirectory {} -> do
    liftIO $ putStrLn "GetHomeDirectory"
    send GetHomeDirectory
  MakeAbsolute filepath -> do
    liftIO $ putStrLn "MakeAbsolute"
    send $ MakeAbsolute filepath
