{-# LANGUAGE BangPatterns #-}
module GLuaFixer.Util where

import GLua.AG.AST
import GLua.AG.PrettyPrint
import qualified GLua.PSLexer as PSL
import qualified GLua.PSParser as PSP
import GLuaFixer.AG.LexLint
import GLuaFixer.LintMessage
import GLuaFixer.LintSettings
import GLuaFixer.BadSequenceFinder

import Control.DeepSeq (force)
import qualified Data.ByteString.Lazy as BS
import System.Exit (exitWith, ExitCode (..))
import System.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory, makeAbsolute, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>), (<.>))
import System.FilePath.GlobPattern (GlobPattern)
import System.FilePath.Find (find, always, fileName, (~~?), (&&?), (/~?))
import System.IO (openFile, withFile, hSetEncoding, utf8_bom, hGetContents, hClose, hPutStr, IOMode (..))
import Text.Parsec.Error (errorPos)

import Control.Applicative ((<|>))
import Data.Aeson (eitherDecode)
import Data.Maybe (fromJust)


-- | Finds all Lua files in a folder
findLuaFiles :: [GlobPattern] -> FilePath -> IO [FilePath]
findLuaFiles ignoreFiles =
    find always (fileName ~~? "*.lua" &&? ignoredGlobs)
  where
    ignoredGlobs = foldl (&&?) always $ map (fileName /~?) ignoreFiles

-- | Read file in utf8_bom because that seems to work better
doReadFile :: FilePath -> IO String
doReadFile f = do
    handle <- openFile f ReadMode
    hSetEncoding handle utf8_bom
    !contents <- force <$> hGetContents handle
    hClose handle
    pure contents

-- | write file in utf8_bom mode
doWriteFile :: FilePath -> String -> IO ()
doWriteFile fp contents = withFile fp WriteMode $ \handle -> do
    hSetEncoding handle utf8_bom
    hPutStr handle contents

-- | Parse a single file using parsec
parseFile :: LintSettings -> FilePath -> String -> Either [LintMessage] ([LintMessage], AST)
parseFile config f contents =
    case PSL.execParseTokens contents of
        Left lexErr ->
            Left [LintError (PSL.sp2Rg $ errorPos lexErr) (renderPSError lexErr) f | lint_syntaxErrors config]

        Right tokens -> do
            let fixedTokens = fixedLexPositions tokens
            let lexWarnings = map ($f) (lintWarnings config fixedTokens ++ sequenceWarnings config fixedTokens)
            let parsed = PSP.parseGLua fixedTokens

            case parsed of
                Left err ->
                    -- Return syntax errors
                    Left [LintError (PSL.sp2Rg $ errorPos err) (renderPSError err) f | lint_syntaxErrors config]
                Right ast ->
                    Right (lexWarnings, ast)


-- | Read the settings from a file
settingsFromFile :: FilePath -> IO (Maybe LintSettings)
settingsFromFile f = do
                        configContents <- BS.readFile f
                        let jsonDecoded = eitherDecode configContents :: Either String LintSettings
                        case jsonDecoded of Left err -> putStrLn (f ++ " [Error] Could not parse config file. " ++ err) >> exitWith (ExitFailure 1)
                                            Right ls -> return $ Just ls

-- | The default settings file
settingsFile :: FilePath
settingsFile = "glualint" <.> "json"

-- | The default settings file when placed in the home folder
homeSettingsFile :: FilePath
homeSettingsFile = ".glualint" <.> "json"

-- | Search upwards in the file path until a settings file is found
searchSettings :: FilePath -> IO (Maybe LintSettings)
searchSettings f = do
    let up = takeDirectory f
    dirExists <- doesDirectoryExist up

    if not dirExists || up == takeDirectory up then
        return Nothing
    else do
        exists <- doesFileExist (f </> settingsFile)
        if exists then
            settingsFromFile (f </> settingsFile)
        else do
            dotExists <- doesFileExist (f </> homeSettingsFile)
            if dotExists then settingsFromFile (f </> homeSettingsFile) else searchSettings up


-- | Look for the file in the home directory
searchHome :: IO (Maybe LintSettings)
searchHome = do
    home <- getHomeDirectory
    exists <- doesFileExist (home </> homeSettingsFile)
    if exists then
        settingsFromFile (home </> homeSettingsFile)
    else do
        nonDotExists <- doesFileExist (home </> settingsFile)
        if nonDotExists then settingsFromFile (home </> settingsFile) else return Nothing

-- | Search the various known places for settings files
getSettings :: FilePath -> IO LintSettings
getSettings f = do
    f' <- if f == "stdin" then getCurrentDirectory else makeAbsolute f
    searchedSettings <- searchSettings f'
    homeSettings <- searchHome
    return . fromJust $ searchedSettings <|> homeSettings <|> Just defaultLintSettings
