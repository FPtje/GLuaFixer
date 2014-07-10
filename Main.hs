module Main where

import Lexer
import TokenTypes
import System.FilePath
import System.Environment
import System.IO

main = do
	args <- getArgs
	let file = head args
	contents <- readFile file
	let lex = alexScanTokens contents

	print . show $ lex
