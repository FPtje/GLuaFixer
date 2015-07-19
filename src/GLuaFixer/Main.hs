module Main where

import GLua.Lexer
import GLua.Parser
import GLua.AG.PrettyPrint
import System.Exit
import System.Environment
import Control.Monad
import System.IO
import Graphics.UI.Gtk
import GLuaFixer.AG.DarkRPRewrite


filePicker :: IO ()
filePicker = do
    initGUI
    window <- windowNew
    set window [windowTitle := "Select Lua file",
               windowDefaultWidth := 800,
               windowDefaultHeight := 600 ]

    fch <- fileChooserWidgetNew FileChooserActionOpen
    containerAdd window fch

    hsfilt <- fileFilterNew
    fileFilterAddPattern hsfilt "*.lua"
    fileFilterSetName hsfilt "Lua file"
    fileChooserAddFilter fch hsfilt

    nofilt <- fileFilterNew
    fileFilterAddPattern nofilt "*.*"
    fileFilterSetName nofilt "All Files"
    fileChooserAddFilter fch nofilt

    onFileActivated fch $
        do dir <- fileChooserGetFilename fch
           case dir of
                Just fpath -> do mainQuit
                                 print fpath
                                 contents <- readFile fpath
                                 fixLua contents
                Nothing -> putStrLn "Nothing"

    widgetShowAll window
    onDestroy window mainQuit
    mainGUI

fixLua :: String -> IO ()
fixLua contents = do
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

main :: IO ()
main = do
    files <- getArgs
    if (not . null $ files) then do
        contents <- readFile . head $ files
        fixLua contents
    else filePicker

