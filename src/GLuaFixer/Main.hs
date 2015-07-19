module Main where

import GLua.Lexer
import GLua.Parser
import GLua.AG.PrettyPrint
import System.Exit
import Control.Monad
import System.IO

import GLuaFixer.AG.DarkRPRewrite

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

    when (not . null . snd $ ast) $ do
        hPutStrLn stderr "Errors:"
        mapM_ (hPrint stderr) . snd $ ast

    putStrLn . prettyprint . fixOldDarkRPSyntax . fst $ ast

    exitSuccess
