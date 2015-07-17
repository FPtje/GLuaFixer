module Main where

import GLua.Lexer
import GLua.Parser
import GLua.AG.PrettyPrint
import System.Exit
import Control.Monad

main :: IO ()
main = do
    contents <- getContents

    -- Lex the file
    let lexed = execParseTokens contents
    let tokens = fst lexed
    let errors = snd lexed

    -- Print any lexing errors
    when (not . null $ errors) $ do
        mapM_ print errors

        exitWith (ExitFailure 1)

    let ast = parseGLua tokens

    putStrLn "Errors:"
    mapM_ print . snd $ ast

    putStrLn "Pretty printed code:"
    putStrLn . prettyprint . fst $ ast

    exitSuccess
