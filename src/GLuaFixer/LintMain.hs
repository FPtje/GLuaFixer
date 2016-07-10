module Main where

import System.Environment
import System.IO
import GLuaFixer.AG.LexLint
import GLua.Parser
import qualified GLua.PSParser as PSP
import qualified GLua.PSLexer as PSL
import GLuaFixer.AG.ASTLint
import GLuaFixer.BadSequenceFinder
import GLua.AG.PrettyPrint
import GLua.AG.AST
import GLua.AG.Token
import System.FilePath
import System.FilePath.Find
import GLuaFixer.LintSettings
import System.Exit
import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Data.Maybe
import System.Directory
import Control.Applicative
import GLuaFixer.AG.DarkRPRewrite
import qualified Data.Map as M
import GHC.Exts (sortWith)

version :: String
version = "1.8.1"

-- | Read file in utf8_bom because that seems to work better
doReadFile :: FilePath -> IO String
doReadFile f = do
    handle <- openFile f ReadMode
    hSetEncoding handle utf8_bom
    hGetContents handle

-- | Pretty print, uses the uu-parsinglib library
prettyPrint :: Maybe Indentation -> IO ()
prettyPrint ind = do
    lua <- getContents

    cwd <- getCurrentDirectory
    lintsettings <- getSettings cwd
    let parsed = parseGLuaFromString lua
    let ast = fst parsed
    let ppconf = lint2ppSetting lintsettings
    let ppconf' = ppconf {indentation = fromMaybe (indentation ppconf) ind}
    let pretty = prettyprintConf ppconf' . fixOldDarkRPSyntax $ ast

    putStr pretty


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
      Left errs -> mapM_ putStrLn errs >> return M.empty
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


-- | Parse a single file using parsec
parseFile :: LintSettings -> FilePath -> String -> Either [String] ([String], AST)
parseFile config f contents =
    case PSL.execParseTokens contents of
        Left lexErr ->
            Left [f ++ ": [Error] " ++ renderPSError lexErr | lint_syntaxErrors config]

        Right tokens -> do
            let fixedTokens = fixedLexPositions tokens
            let lexWarnings = map ((f ++ ": ") ++) (lintWarnings config fixedTokens ++ sequenceWarnings config fixedTokens)
            let parsed = PSP.parseGLua fixedTokens

            case parsed of
                Left err ->
                    -- Return syntax errors
                    Left [f ++ ": [Error] " ++ renderPSError err | lint_syntaxErrors config]
                Right ast -> Right (lexWarnings, ast)


-- | Lint a single file, using parsec
lintFile :: LintSettings -> FilePath -> String -> [String]
lintFile config f contents =
    case parseFile config f contents of
        Left errs -> errs
        Right (lexWarnings, ast) ->
            let
                parserWarnings = map ((f ++ ": ") ++) $ astWarnings config ast
            in
                -- Print all warnings
                lexWarnings ++ parserWarnings


-- | Finds all Lua files
findLuaFiles :: FilePath -> IO [FilePath]
findLuaFiles = find always (fileName ~~? "*.lua")

-- | Lint a set of files, uses parsec's parser library
lint :: Maybe LintSettings -> [FilePath] -> IO ()
lint _ [] = return ()
lint ls (f : fs) = do
    settings <- getSettings f
    let config = fromJust $ ls <|> Just settings

    -- When we're dealing with a directory, lint all the files in it recursively.
    isDirectory <- doesDirectoryExist f

    if isDirectory then do
        luaFiles <- findLuaFiles f
        lint ls luaFiles
    else if f == "stdin" then do
        contents <- getContents

        mapM_ putStrLn (lintFile config f contents)
    else do
        contents <- doReadFile f

        mapM_ putStrLn (lintFile config f contents)

    -- Lint the other files
    lint ls fs

-- | Read the settings from a file
settingsFromFile :: FilePath -> IO (Maybe LintSettings)
settingsFromFile f = do
                        configContents <- BS.readFile f
                        let jsonDecoded = eitherDecode configContents :: Either String LintSettings
                        case jsonDecoded of Left err -> putStrLn (f ++ " [Error] Could not parse config file. " ++ err) >> exitWith (ExitFailure 1)
                                            Right ls -> return $ Just ls

type Indentation = String

-- | Simple argument parser
parseCLArgs :: Maybe Indentation -> [String] -> IO (Maybe LintSettings, [FilePath])
parseCLArgs _ [] = return (Nothing, [])
parseCLArgs ind ("--pretty-print" : _) = prettyPrint ind >> exitSuccess
parseCLArgs _ ("--analyse-globals" : f : _) = analyseGlobals f >> exitSuccess
parseCLArgs _ ("--version" : _) = putStrLn version >> exitSuccess
parseCLArgs ind ("--stdin" : xs) = do
                                 (sets, pths) <- parseCLArgs ind xs
                                 return (sets, "stdin" : pths)
parseCLArgs _ ["--config"] = putStrLn "Well give me a config file then you twat" >> exitWith (ExitFailure 1)
parseCLArgs ind ("--config" : f : xs) = do
                                        settings <- settingsFromFile f
                                        (_, fps) <- parseCLArgs ind xs
                                        return (settings, fps)

parseCLArgs _ (('-' : '-' : 'i' : 'n' : 'd' : 'e' : 'n' : 't' : 'a' : 't' : 'i' : 'o' : 'n' : '=' : '\'' : ind) : xs) = parseCLArgs (Just (init ind)) xs
-- I didn't think this function would get this complex task...
parseCLArgs _ (('-' : '-' : 'i' : 'n' : 'd' : 'e' : 'n' : 't' : 'a' : 't' : 'i' : 'o' : 'n' : '=' : ind) : xs) = parseCLArgs (Just ind) xs
parseCLArgs ind (f : xs) = do (ls, fs) <- parseCLArgs ind xs
                              return (ls, f : fs)

settingsFile :: FilePath
settingsFile = "glualint" <.> "json"

homeSettingsFile :: FilePath
homeSettingsFile = ".glualint" <.> "json"

-- Search upwards in the file path until a settings file is found
searchSettings :: FilePath -> IO (Maybe LintSettings)
searchSettings f = do
                        let up = takeDirectory f
                        dirExists <- doesDirectoryExist up

                        if not dirExists || up == takeDirectory up then
                            return Nothing
                        else do
                            exists <- doesFileExist (f </> settingsFile)
                            if exists then
                                settingsFromFile (f </> settingsFile)
                            else do
                                dotExists <- doesFileExist (f </> homeSettingsFile)
                                if dotExists then settingsFromFile (f </> homeSettingsFile) else searchSettings up


-- Look for the file in the home directory
searchHome :: IO (Maybe LintSettings)
searchHome = do
                home <- getHomeDirectory
                exists <- doesFileExist (home </> homeSettingsFile)
                if exists then
                    settingsFromFile (home </> homeSettingsFile)
                else do
                    nonDotExists <- doesFileExist (home </> settingsFile)
                    if nonDotExists then settingsFromFile (home </> settingsFile) else return Nothing

-- | Search the various known places for settings files
getSettings :: FilePath -> IO LintSettings
getSettings f = do
    searchedSettings <- searchSettings f
    homeSettings <- searchHome
    return . fromJust $ searchedSettings <|> homeSettings <|> Just defaultLintSettings

-- | Main function
main :: IO ()
main = do
    args <- getArgs
    (settings, files) <- parseCLArgs Nothing args

    lint settings files
