module Main where

import System.Environment
import System.IO
import Control.Monad
import GLua.Lexer
import GLuaFixer.AG.LexLint
import GLua.Parser
import GLuaFixer.AG.ASTLint
import System.FilePath.Posix
import GLuaFixer.LintSettings
import System.Exit
import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Data.Maybe
import System.Directory
import Control.Applicative

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
lint :: LintSettings -> [FilePath] -> IO ()
lint _ [] = return ()
lint ls (f : fs) = do
    contents <- doReadFile f

    let lexed = execParseTokens contents
    let tokens = fst lexed
    let warnings = map ((++) (takeFileName f ++ ": ")) $ lintWarnings tokens

    -- Fixed for positions
    let fixedTokens = fixedLexPositions tokens
    let parsed = parseGLua fixedTokens
    let ast = fst parsed
    let parserWarnings = map ((++) (takeFileName f ++ ": ")) $ astWarnings ast

    -- Print all warnings
    when (null . snd $ lexed) $ do
        mapM_ putStrLn warnings

        when (null . snd $ parsed) $ do
            mapM_ putStrLn parserWarnings

    -- Lint the other files
    lint ls fs

settingsFromFile :: FilePath -> IO (Maybe LintSettings)
settingsFromFile f = do
                        configContents <- BS.readFile f
                        let jsonDecoded = eitherDecode configContents :: Either String LintSettings
                        case jsonDecoded of Left err -> putStrLn (f ++ " [Error] Could not parse config file. " ++ err) >> exitWith (ExitFailure 1)
                                            Right ls -> return $ Just ls

parseCLArgs :: [String] -> IO (Maybe LintSettings, [FilePath])
parseCLArgs [] = return (Nothing, [])
parseCLArgs ("--version" : _) = putStrLn version >> exitWith ExitSuccess
parseCLArgs ("--config" : []) = putStrLn "Well give me a config file then you twat" >> exitWith (ExitFailure 1)
parseCLArgs ("--config" : f : xs) = do
                                        settings <- settingsFromFile f
                                        (_, fps) <- parseCLArgs xs
                                        return (settings, fps)
parseCLArgs (f : xs) = do (ls, fs) <- parseCLArgs xs
                          return (ls, f : fs)

settingsFile :: FilePath
settingsFile = "glualint" <.> "json"

-- Search upwards in the file path until a settings file is found
searchSettings :: FilePath -> IO (Maybe LintSettings)
searchSettings f = do
                        dirExists <- doesDirectoryExist f
                        let up = takeDirectory f
                        if not dirExists || up == takeDirectory up then
                            return Nothing
                        else do
                            exists <- doesFileExist (f </> settingsFile)
                            if exists then
                                settingsFromFile (f </> settingsFile)
                            else
                                searchSettings up


main :: IO ()
main = do
    cwd <- getCurrentDirectory
    args <- getArgs
    (settings, files) <- parseCLArgs args
    searchedSettings <- searchSettings cwd
    let config = fromJust $ settings <|> searchedSettings <|> Just defaultLintSettings

    lint config files
