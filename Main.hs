module Main where

import GLua.Lexer
import GLua.TokenTypes
import System.FilePath
import System.Environment
import System.IO
import Control.Monad

main = do
    args <- getArgs
    let file = head args
    contents <- readFile file
    let lex = execParseTokens contents
    let newcontents = show . makeMTokens $ fst lex
    putStrLn newcontents

    unless (null $ snd lex) $
        print . snd $ lex

