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
import System.IO (hPutStrLn, stderr)


version :: String
version = "1.9.11"


-- | Pretty print, uses the uu-parsinglib library
prettyPrintStdin :: Maybe Indentation -> IO ()
prettyPrintStdin ind = do
    lua <- getContents

    cwd <- getCurrentDirectory
    lintsettings <- getSettings cwd

    putStr $ prettyPrint ind lintsettings lua

prettyPrintFiles :: Maybe Indentation -> [FilePath] -> IO ()
prettyPrintFiles ind = mapM_ pp
  where
    pp :: FilePath -> IO ()
    pp f = do
      isDirectory <- doesDirectoryExist f
      if isDirectory then do
        luaFiles <- findLuaFiles f
        prettyPrintFiles ind luaFiles
      else do
        hPutStrLn stderr $ "Pretty printing " ++ f
        lintsettings <- getSettings f
        lua <- doReadFile f
        doWriteFile f $ prettyPrint ind lintsettings lua

-- | Pure pretty print function
prettyPrint :: Maybe Indentation -> LintSettings -> String -> String
prettyPrint ind lintsettings lua =
    prettyprintConf ppconf' . fixOldDarkRPSyntax $ ast
  where
    (ast, _errors) = parseGLuaFromString lua
    ppconf = lint2ppSetting lintsettings
    ppconf' = ppconf {indentation = fromMaybe (indentation ppconf) ind}

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
parseCLArgs ind ("--pretty-print-files" : fs) = prettyPrintFiles ind fs >> exitSuccess
parseCLArgs ind ("--pretty-print" : _) = prettyPrintStdin ind >> exitSuccess
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
