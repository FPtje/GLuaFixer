module Main where

import GLua.Lexer
import GLua.TokenTypes
import GLua.Parser
import GLua.AG.PrettyPrint

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
 "    fix       - attempt to fix syntax errors in the Lua script"
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
        -- Attempt to fix errors when asked
        when (map toLower action == "fix") $ do
            writeFile file . concatMap show $ tokens
            putStrLn "Success"
            exitSuccess

        exitWith (ExitFailure 1)

    let ast = parseGLua tokens

    putStrLn "Errors:"
    mapM_ (putStrLn . renderError) . snd $ ast

    putStrLn "Pretty printed code:"
    putStrLn . prettyprint . fst $ ast

    exitSuccess
