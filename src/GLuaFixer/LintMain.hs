module Main where

import "glualint-lib" GLua.AG.PrettyPrint
import "glualint-lib" GLua.Parser
import "glualint-lib" GLua.ASTInstances ()
import "glualint-lib" GLua.TokenTypes (isWhitespace)
import "glualint-lib" GLuaFixer.AG.ASTLint
import "glualint-lib" GLuaFixer.AG.DarkRPRewrite
import "glualint-lib" GLuaFixer.AnalyseProject
import "glualint-lib" GLuaFixer.LintMessage
import "glualint-lib" GLuaFixer.LintSettings
import "glualint-lib" GLuaFixer.Util

import qualified "glualint-lib" GLua.Lexer as Lex

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Maybe (fromMaybe, fromJust)
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitWith, exitSuccess, exitFailure, ExitCode (..))
import System.IO (hPutStrLn, stderr)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BL
import Data.Foldable

version :: String
version = "1.16.3"

-- | Pretty print, uses the uu-parsinglib library
prettyPrintStdin :: Maybe Indentation -> IO ()
prettyPrintStdin ind = do
    lua <- getContents

    cwd <- getCurrentDirectory
    lintsettings <- getSettings cwd

    for_ (prettyPrint ind lintsettings lua) $ \result ->
        putStr result

prettyPrintFiles :: Maybe Indentation -> [FilePath] -> IO ()
prettyPrintFiles ind = mapM_ pp
  where
    pp :: FilePath -> IO ()
    pp f = do
      isDirectory <- doesDirectoryExist f
      if isDirectory then do
        luaFiles <- findLuaFiles [] f
        prettyPrintFiles ind luaFiles
      else do
        hPutStrLn stderr $ "Pretty printing " ++ f
        lintsettings <- getSettings f
        lua <- doReadFile f
        for_ (prettyPrint ind lintsettings lua) $ \result ->
            doWriteFile f result

-- | Pure pretty print function
prettyPrint :: Maybe Indentation -> LintSettings -> String -> Maybe String
prettyPrint ind lintsettings lua =
    if prettyprint_rejectInvalidCode lintsettings && not (null errors)
    then Nothing
    else Just $ prettyprintConf ppconf' $ fixOldDarkRPSyntax ast
  where
    (ast, errors) = parseGLuaFromString lua
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
-- Bool return value indicates whether there were either warnings or errors
lint :: Maybe LintSettings -> [FilePath] -> IO Bool
lint _ [] = pure False
lint ls (f : fs) = do
    settings <- getSettings f
    let config = fromJust $ ls <|> Just settings

    -- When we're dealing with a directory, lint all the files in it recursively.
    isDirectory <- doesDirectoryExist f

    hasMsgs <- if isDirectory then findLuaFiles (lint_ignoreFiles config) f >>= lint ls
        else if f == "stdin" then do
            msgs <- lintFile config f <$> getContents

            mapM_ print msgs
            pure $ not $ null msgs
        else do
            msgs <- lintFile config f <$> doReadFile f

            mapM_ print msgs
            pure $ not $ null msgs

    -- Lint the other files
    (|| hasMsgs) <$> lint ls fs

dumpAST :: [FilePath] -> IO ()
dumpAST fs =
    for_ fs $ \f -> do
        isDirectory <- doesDirectoryExist f
        if isDirectory then findLuaFiles [] f >>= dumpAST
        else do
            contents <- if f == "stdin" then getContents else doReadFile f
            config <- getSettings f

            case parseFile config f contents of
                Left errs -> mapM_ print errs
                Right (_lexWarnings, ast) ->
                    BL.putStr $ JSON.encode ast

runTest :: [FilePath] -> IO ()
runTest fs = do
    putStrLn "Running tests"
    for_ fs $ \f -> do
        isDirectory <- doesDirectoryExist f
        if isDirectory then do
          luaFiles <- findLuaFiles [] f
          runTest luaFiles
        else do
            contents <- doReadFile f
            let (uu_lex, uu_lex_errors) = Lex.execParseTokens contents

            unless (null uu_lex_errors) $ do
                putStrLn $ "Errors when trying to lex '" ++ f ++
                    "' with uu-parsinglib lexer!"
                print uu_lex_errors

            let (uu_ast, uu_parseErrs) = parseGLua $ filter (not . isWhitespace) uu_lex
            lintsettings <- getSettings f
            putStrLn $ "Testing " ++ f
            unless (null uu_parseErrs) $ do
                putStrLn $ "Errors when trying to parse '" ++ f ++
                    "' with uu-parsinglib parser!"
                print uu_parseErrs

            case parseFile lintsettings f contents of
                Right _ -> pure ()
                Left errs -> do
                    putStrLn $ "Errors when trying to parse '" ++ f ++ "' with parsec parser!"
                    print errs

            let pretty_printed = prettyprintConf (lint2ppSetting lintsettings) $ fixOldDarkRPSyntax uu_ast
            let (_uu_ast_pp, uu_parseErrs_pp) = parseGLuaFromString pretty_printed
            unless (null uu_parseErrs_pp) $ do
                putStrLn $ "Errors when trying to parse '" ++ f ++
                    "' with uu-parsinglib parser after pretty print!"
                print uu_parseErrs

            case parseFile lintsettings f pretty_printed of
                Right _ -> pure ()
                Left errs -> do
                    putStrLn $ "Errors when trying to parse '" ++ f ++
                        "' with parsec parser after pretty print!"
                    print errs

-- | Indentation used for pretty printing code
type Indentation = String


-- | Simple argument parser
parseCLArgs :: Maybe Indentation -> [String] -> IO (Maybe LintSettings, [FilePath])
parseCLArgs _ [] = return (Nothing, [])
parseCLArgs ind ("--pretty-print-files" : fs) = prettyPrintFiles ind fs >> exitSuccess
parseCLArgs ind ("--pretty-print" : _) = prettyPrintStdin ind >> exitSuccess
parseCLArgs _ ("--analyse-globals" : fs) = analyseGlobals fs >> exitSuccess
parseCLArgs _ ("--dump-ast" : fs) = dumpAST fs >> exitSuccess
parseCLArgs _ ("--version" : _) = putStrLn version >> exitSuccess
parseCLArgs _ ("--test" : fs) = runTest fs >> exitSuccess
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

    hasMessages <- lint settings files
    if hasMessages then exitFailure
    else exitSuccess
