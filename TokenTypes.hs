module TokenTypes where

data Token =
	Whitespace String			|
	DashComment String			|
	SlashComment String			|
	Function

	deriving (Show)
