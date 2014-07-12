module Main where

import Lexer
import TokenTypes
import System.FilePath
import System.Environment
import System.IO
import Control.Monad

main = do
    args <- getArgs
    let file = head args
    contents <- readFile file
    let lex = execParseTokens contents
    let newcontents = concatMap show $ fst lex

    unless (null $ snd lex) $
        print . snd $ lex

