module Main where

import Lexer2
import TokenTypes
import System.FilePath
import System.Environment
import System.IO

main = do
	args <- getArgs
	let file = head args
	contents <- readFile file
	let lex = execParseTokens contents

	print . show $ lex
