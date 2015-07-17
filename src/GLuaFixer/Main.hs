module Main where

import GLua.Lexer
import GLua.Parser
import GLua.AG.PrettyPrint

import Data.Char

import System.Environment
import System.Exit
import Control.Monad

help :: String
help = unlines ["",
 "Usage: gluafixer <FILE>"
 ]


main :: IO ()
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
    let lexed = execParseTokens contents
    let tokens = fst lexed
    let errors = snd lexed

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
    mapM_ print . snd $ ast

    putStrLn "Pretty printed code:"
    putStrLn . prettyprint . fst $ ast

    exitSuccess
