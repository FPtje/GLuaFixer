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

-- | Read file in utf8_bom because that seems to work better
doReadFile :: FilePath -> IO String
doReadFile f = do
    handle <- openFile f ReadMode
    hSetEncoding handle utf8_bom
    contents <- hGetContents handle
    return contents

-- | Write file in utf8_bom
doWriteFile :: FilePath -> String -> IO ()
doWriteFile f str = do
    handle <- openFile f WriteMode
    hSetEncoding handle utf8_bom
    hPutStr handle str
    hClose handle

filePicker :: IO ()
filePicker = do
    initGUI

    dialog <- fileChooserDialogNew (Just "Open Lua File") Nothing FileChooserActionOpen [("Cancel", ResponseCancel), ("Open", ResponseAccept)]

    widgetShow dialog
    resp <- dialogRun dialog

    case resp of
          ResponseCancel -> widgetDestroy dialog
          ResponseAccept -> do nwf <- fileChooserGetFilename dialog
                               case nwf of
                                    Nothing -> widgetDestroy dialog
                                    Just path -> do
                                        widgetDestroy dialog
                                        contents <- doReadFile path
                                        fixLua contents path  []
          ResponseDeleteEvent -> widgetDestroy dialog

    widgetDestroy dialog

saveFixed :: String -> IO ()
saveFixed s = do
    initGUI
    dialog <- fileChooserDialogNew (Just "Save Fixed Lua File") Nothing FileChooserActionSave [("Cancel", ResponseCancel), ("Save", ResponseAccept)]
    fileChooserSetCurrentName dialog "Fixed.lua"
    fileChooserSetDoOverwriteConfirmation dialog True
    widgetShow dialog
    resp <- dialogRun dialog

    case resp of
          ResponseCancel -> widgetDestroy dialog
          ResponseAccept -> do nwf <- fileChooserGetFilename dialog
                               case nwf of
                                    Nothing -> widgetDestroy dialog
                                    Just path -> do
                                        doWriteFile path s
                                        widgetDestroy dialog
          ResponseDeleteEvent -> widgetDestroy dialog

    widgetDestroy dialog



fixLua :: String -> String -> [String] -> IO ()
fixLua contents fileName args = do
    -- Lex the file
    let lexed = execParseTokens contents
    let tokens = fst lexed
    let lexErrors = map renderError (snd lexed)
    let ast = parseGLua tokens
    let fixed = prettyprint . fixOldDarkRPSyntax . fst $ ast

    if length args < 2 then
        saveFixed fixed
    else
        putStrLn fixed

    let errors = lexErrors ++ (map renderError $ snd ast)
    when (not . null $ errors) $ do
        hPutStrLn stderr "Errors:"
        mapM_ (hPutStrLn stderr) $ errors
        doWriteFile "LuaErrors.txt" $ "Errors in " ++ fileName ++ ":\n" ++ concatMap (++ "\n") errors

        when (length args < 2) $ do
            initGUI
            infoDialog <- messageDialogNew Nothing [] MessageWarning ButtonsClose (fileName ++ " contains some errors!\nPlease see LuaErrors.txt!")
            widgetShow infoDialog
            dialogRun infoDialog
            widgetDestroy infoDialog


    when (null errors) $ do
        doWriteFile "LuaErrors.txt" $ "No errors found in " ++ fileName

        when (length args < 2) $ do
            initGUI
            infoDialog <- messageDialogNew Nothing [] MessageInfo ButtonsClose (fileName ++ " pretty printed correctly!")
            widgetShow infoDialog
            dialogRun infoDialog
            widgetDestroy infoDialog

main :: IO ()
main = do
    files <- getArgs
    if (not . null $ files) then do
        let f = head files
        contents <- doReadFile $ f
        fixLua contents f files
    else filePicker

