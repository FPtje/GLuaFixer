module Main where

import System.Environment
import System.IO
import Control.Monad
import GLua.Lexer
import GLuaFixer.AG.LexLint
import GLua.Parser
import GLuaFixer.AG.ASTLint
import Data.String.Utils
import System.FilePath.Posix

version :: String
version = "1.0.0"

-- | Read file in utf8_bom because that seems to work better
doReadFile :: FilePath -> IO String
doReadFile f = do
    handle <- openFile f ReadMode
    hSetEncoding handle utf8_bom
    contents <- hGetContents handle
    return contents


-- | Lint a set of files
lint :: [FilePath] -> IO ()
lint [] = return ()
lint (f : fs) = do
    contents <- doReadFile f

    let tabsToSpaces = replace "\t" " " contents

    let lexed = execParseTokens tabsToSpaces
    let tokens = fst lexed
    let warnings = map ((++) (takeFileName f ++ ": ")) $ lintWarnings tokens
    let parsed = parseGLua tokens
    let ast = fst parsed
    let parserWarnings = map ((++) (takeFileName f ++ ": ")) $ astWarnings ast

    -- Print all warnings
    when (null . snd $ lexed) $ do
        mapM_ putStrLn warnings

        when (null . snd $ parsed) $ do
            mapM_ putStrLn parserWarnings

    -- Lint the other files
    lint fs

main :: IO ()
main = do
    args <- getArgs
    if head args == "--version" then
        putStrLn version
    else
        lint args
