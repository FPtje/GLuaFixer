module Main where

import GLua.Lexer
import GLua.TokenTypes
import GLua.Parser
import qualified GLua.PSParser as PS
import GLua.AG.PrettyPrint
import GLua.AG.AST

import Data.Char

import System.FilePath
import System.Environment
import System.IO
import System.Exit
import Control.Monad
import Text.Parsec

help = unlines ["",
 "Usage: GLuaParser <ACTION> <FILE>",
 "",
 "Possible actions:",
 "    fix       - attempt to fix syntax errors in the Lua script"
 ]


printErrors :: ParseError -> IO ()
printErrors ps = do
    putStrLn "Errors:"
    putStrLn . renderPSError $ ps

printAST :: AST -> IO ()
printAST ast = do
    putStrLn "Pretty printed code:"
    putStrLn . prettyprint $ ast

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

    let ast = PS.parseGLua tokens

    either printErrors printAST ast

    {-
    putStrLn "Errors:"
    mapM_ (putStrLn . renderError) . snd $ ast

    putStrLn "Pretty printed code:"
    putStrLn . prettyprint . fst $ ast

    -- -}

    exitSuccess
