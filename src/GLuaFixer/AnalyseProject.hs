module GLuaFixer.AnalyseProject where

import GLua.AG.PrettyPrint
import GLua.AG.Token
import GLuaFixer.AG.ASTLint
import GLuaFixer.LintMessage
import GLuaFixer.LintSettings
import GLuaFixer.Util

import GHC.Exts (sortWith)
import qualified Data.Map as M
import System.Directory (doesDirectoryExist)

-- | Represent the places where globals were found in a file
data GlobalAnalysis =
  GlobalAnalysis
    { ga_path :: FilePath
    , ga_regions :: [Region]
    }

  deriving (Show)

-- | Analyse the globals of a single file
analyseGlobalsFile :: LintSettings -> FilePath -> IO (M.Map String [GlobalAnalysis])
analyseGlobalsFile lintSettings f =
  do
    contents <- readFile f

    case parseFile lintSettings f contents of
      Left errs -> mapM_ print (sortLintMessages errs) >> return M.empty
      Right (_, ast) ->
          return $ M.map ((:[]) . GlobalAnalysis f) $ globalDefinitions lintSettings ast

-- | Analyse the globals of a path
analyseGlobals :: FilePath -> IO ()
analyseGlobals f =
  do
    settings <- getSettings f

    -- When we're dealing with a directory, lint all the files in it recursively.
    isDirectory <- doesDirectoryExist f

    if isDirectory then
      do
        luaFiles <- findLuaFiles f
        globals <- mapM (analyseGlobalsFile settings) luaFiles
        let globals' = M.unionsWith (++) globals
        reportGlobals globals'
    else
      do
        globals <- analyseGlobalsFile settings f
        reportGlobals globals

-- | Report found global variables
reportGlobals :: M.Map String [GlobalAnalysis] -> IO ()
reportGlobals globals =
  let
    globals' :: [(String, [GlobalAnalysis])]
    globals' = M.toList $ M.map (sortWith ga_path) globals

    reportRegions :: [Region] -> IO ()
    reportRegions rgs =
      mapM_ (\r -> putStrLn $ "    " ++ renderRegion r) $ reverse rgs

    reportAnalysis :: GlobalAnalysis -> IO ()
    reportAnalysis analysis =
      do
        putStrLn $ "  " ++ ga_path analysis ++ ":"
        reportRegions $ ga_regions analysis
        putStrLn ""

    reportGlobal :: (String, [GlobalAnalysis]) -> IO ()
    reportGlobal (global, analyses) =
      do
        putStrLn $ "- " ++ global
        mapM_ reportAnalysis analyses
  in
    mapM_ reportGlobal globals'
