module Main where

import System.Environment
import System.IO
import Control.Monad
import GLua.Lexer
import GLuaFixer.AG.LexLint

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
    let lexicon = fst lexed
    let warnings = lintWarnings lexicon

    -- Print all warnings
    when (null . snd $ lexed) $ do
        mapM_ putStrLn warnings


    -- Lint the other files
    lint fs

main :: IO ()
main = do
    args <- getArgs
    lint args
