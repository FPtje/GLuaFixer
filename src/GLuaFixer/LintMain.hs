module Main where

import System.Environment
import System.IO
import Control.Monad
import GLua.Lexer
import GLuaFixer.AG.LexLint
import GLua.Parser

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
    let lexed = execParseTokens contents
    let tokens = fst lexed
    let warnings = lintWarnings tokens
    let parsed = parseGLua tokens
    let ast = fst parsed

    -- Print all warnings
    when (null . snd $ lexed) $ do
        mapM_ putStrLn warnings

        when (null . snd $ parsed) $ do
            putStrLn "yis"

    -- Lint the other files
    lint fs

main :: IO ()
main = do
    args <- getArgs
    lint args
