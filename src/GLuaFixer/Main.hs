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

    dialog <- fileChooserDialogNew (Just "Open Lua File") Nothing FileChooserActionOpen [("Cancel", ResponseCancel), ("Open", ResponseAccept)]

    widgetShow dialog
    response <- dialogRun dialog

    case response of
          ResponseCancel -> widgetDestroy dialog
          ResponseAccept -> do nwf <- fileChooserGetFilename dialog
                               case nwf of
                                    Nothing -> widgetDestroy dialog
                                    Just path -> do
                                        widgetDestroy dialog
                                        contents <- readFile path
                                        fixLua contents
          ResponseDeleteEvent -> widgetDestroy dialog

    widgetDestroy dialog

saveFixed :: String -> IO ()
saveFixed s = do
    initGUI
    dialog <- fileChooserDialogNew (Just "Save Fixed Lua File") Nothing FileChooserActionSave [("Cancel", ResponseCancel), ("Save", ResponseAccept)]
    fileChooserSetCurrentName dialog "Fixed.lua"
    fileChooserSetDoOverwriteConfirmation dialog True
    widgetShow dialog
    response <- dialogRun dialog

    case response of
          ResponseCancel -> widgetDestroy dialog
          ResponseAccept -> do nwf <- fileChooserGetFilename dialog
                               case nwf of
                                    Nothing -> widgetDestroy dialog
                                    Just path -> do
                                        writeFile path s
                                        widgetDestroy dialog
          ResponseDeleteEvent -> widgetDestroy dialog

    widgetDestroy dialog



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

    let fixed = prettyprint . fixOldDarkRPSyntax . fst $ ast

    saveFixed fixed

main :: IO ()
main = do
    files <- getArgs
    if (not . null $ files) then do
        contents <- readFile . head $ files
        fixLua contents
    else filePicker

