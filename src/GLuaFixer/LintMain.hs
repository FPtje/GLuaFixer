module Main where

import "glualint-lib" GLua.AG.PrettyPrint
import "glualint-lib" GLua.Parser
import "glualint-lib" GLuaFixer.AG.ASTLint
import "glualint-lib" GLuaFixer.AG.DarkRPRewrite
import "glualint-lib" GLuaFixer.AnalyseProject
import "glualint-lib" GLuaFixer.LintMessage
import "glualint-lib" GLuaFixer.LintSettings
import "glualint-lib" GLuaFixer.Util

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, fromJust)
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitWith, exitSuccess, ExitCode (..))


version :: String
version = "1.9.4"


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
lintFile :: LintSettings -> FilePath -> String -> [LintMessage]
lintFile config f contents =
    case parseFile config f contents of
        Left errs -> errs
        Right (lexWarnings, ast) ->
            let
                parserWarnings = map ($f) $ astWarnings config ast
            in
                -- Print all warnings
                sortLintMessages $ lexWarnings ++ parserWarnings


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

        mapM_ print (lintFile config f contents)
    else do
        contents <- doReadFile f

        mapM_ print (lintFile config f contents)

    -- Lint the other files
    lint ls fs

-- | Indentation used for pretty printing code
type Indentation = String


-- | Simple argument parser
parseCLArgs :: Maybe Indentation -> [String] -> IO (Maybe LintSettings, [FilePath])
parseCLArgs _ [] = return (Nothing, [])
parseCLArgs ind ("--pretty-print" : _) = prettyPrint ind >> exitSuccess
parseCLArgs _ ("--analyse-globals" : fs) = analyseGlobals fs >> exitSuccess
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


-- | Main function
main :: IO ()
main = do
    args <- getArgs
    (settings, files) <- parseCLArgs Nothing args

    lint settings files
