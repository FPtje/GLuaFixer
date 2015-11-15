module Main where

import System.Environment
import System.IO
import Control.Monad
import GLuaFixer.AG.LexLint
import GLua.Parser
import qualified GLua.PSParser as PSP
import qualified GLua.PSLexer as PSL
import GLuaFixer.AG.ASTLint
import GLuaFixer.BadSequenceFinder
import GLua.AG.PrettyPrint
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

version :: String
version = "1.5.1"

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


-- | Lint a single file, using parsec
lintFile :: LintSettings -> FilePath -> String -> [String]
lintFile config f contents =
    case PSL.execParseTokens contents of
        Left lexErr ->
            [f ++ ": [Error] " ++ renderPSError lexErr | lint_syntaxErrors config]

        Right tokens -> do
            let lexWarnings = map ((f ++ ": ") ++) (lintWarnings config tokens ++ sequenceWarnings config tokens)
            let parsed = PSP.parseGLua tokens

            case parsed of
                Left err ->
                    -- Return syntax errors
                    [f ++ ": [Error] " ++ renderPSError err | lint_syntaxErrors config]

                Right ast -> do
                    let parserWarnings = map ((f ++ ": ") ++) $ astWarnings config ast

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
parseCLArgs _ ("--version" : _) = putStrLn version >> exitSuccess
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
