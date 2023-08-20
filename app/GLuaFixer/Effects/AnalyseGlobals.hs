{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GLuaFixer.Effects.AnalyseGlobals where

import Data.Char (toLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Effectful (Eff, (:>))
import qualified Effectful.State.Static.Local as State
import GHC.Exts (sortWith)
import GLua.AG.AST (AST)
import GLua.AG.PrettyPrint (renderRegion)
import GLua.AG.Token (Region)
import GLuaFixer.AG.ASTLint (globalDefinitions)
import GLuaFixer.Effects.Logging (Logging, putStrLnStdOut)
import GLuaFixer.LintSettings (LintSettings)
import Prelude hiding (readFile, writeFile)

data VariableLocation = VariableLocation
  { file :: !FilePath
  , regions :: ![Region]
  }
newtype AnalysisState = AnalysisState {state :: Map String [VariableLocation]}

emptyState :: AnalysisState
emptyState = AnalysisState Map.empty

-- | Run the analysis and get the finished analysis state
execAnalysis :: Eff (State.State AnalysisState : es) () -> Eff es AnalysisState
execAnalysis = State.execState emptyState

-- | Analyse a single file
analyseFile :: State.State AnalysisState :> es => LintSettings -> FilePath -> AST -> Eff es ()
analyseFile lintSettings filepath ast = do
  let
    definitionsInFile :: Map String [Region] = globalDefinitions lintSettings ast
    variableLocations :: Map String [VariableLocation] =
      Map.map ((: []) . VariableLocation filepath) definitionsInFile

  State.modify $ \analysisState ->
    AnalysisState $ Map.unionWith (++) analysisState.state variableLocations

-- | Print the analysis state to the terminal.
-- TODO: make the rendering a pure function
reportAnalysis :: forall es. Logging :> es => AnalysisState -> Eff es ()
reportAnalysis analysis =
  mapM_ reportGlobal globals
  where
    globals :: [(String, [VariableLocation])]
    globals = sortWith (map toLower . fst) $ Map.toList $ Map.map (sortWith file) analysis.state

    reportRegions :: [Region] -> Eff es ()
    reportRegions rgs =
      mapM_ (\r -> putStrLnStdOut $ "    " ++ renderRegion r) $ reverse rgs

    reportVariableLocation :: VariableLocation -> Eff es ()
    reportVariableLocation location =
      do
        putStrLnStdOut $ "  " ++ file location ++ ":"
        reportRegions $ regions location
        putStrLnStdOut ""

    reportGlobal :: (String, [VariableLocation]) -> Eff es ()
    reportGlobal (global, analyses) =
      do
        putStrLnStdOut $ "- " ++ global
        mapM_ reportVariableLocation analyses
