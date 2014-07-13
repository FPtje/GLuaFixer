module Main where

import GLua.Lexer
import GLua.TokenTypes
import LexicalAnalysis

import Data.Char

import System.FilePath
import System.Environment
import System.IO
import System.Exit
import Control.Monad

help = unlines ["",
 "Usage: GLuaParser <ACTION> <FILE>",
 "",
 "Possible actions:",
 "    toPureLua - convert the FILE to use pure Lua operators and comments (~=, --, etc.)",
 "    toGLua    - convert the FILE to use Garry's mod Lua operators and comments (!=, //, etc.)"
 ]


main = do
    args <- getArgs

    -- Argument checking
    when (length args < 2) $ do
        putStrLn help
        exitSuccess

    let action = head args
    let file = args !! 1

    contents <- readFile file

    -- Lex the file
    let lex = execParseTokens contents
    let tokens = fst lex
    let errors = snd lex

    -- Print any lexing errors
    unless (null errors) $ do
        mapM_ print errors
        exitWith (ExitFailure 1)

    when (map toLower action == "topurelua") $ do
        writeFile file . concatMap show . toPureLua $ tokens
        exitSuccess

    when (map toLower action == "toglua") $ do
        writeFile file . concatMap show . toGLua $ tokens
        exitSuccess

