module TokenTypes where

data Token =
	Whitespace String			|
	DashComment String			|
	DashBlockComment String		|
	SlashComment String			|
	SlashBlockComment String	|

	TNumber String				|

	-- Keywords
	And							|
	Break						|
	Do							|
	Else						|
	Elseif						|
	End							|
	TFalse						|
	For							|
	Function					|
	Goto						|
	If							|
	In							|
	Local						|
	Nil							|
	Not							|
	Or							|
	Repeat						|
	Return						|
	Then						|
	TTrue						|
	Until						|
	While						|

	Plus						|	-- +
	Minus						|	-- -
	Mulitply					|	-- *
	Divide						|	-- /
	Modulus						|	-- %
	Power						|	-- ^
	Hash						|	-- #
	TEq							|	-- ==
	TNEq						|	-- ~=
	TLEQ						|	-- <=
	TGEQ						|	-- >=
	TLT							|	-- <
	TGT							|	-- >
	Equals						|	-- =
	LRound						|	-- (
	RRound						|	-- )
	LCurly						|	-- {
	RCurly						|	-- }
	LSquare						|	-- [
	RSquare 					|	-- ]
	Label String				|	-- ::
	Semicolon					|	-- ;
	Colon						|	-- :
	Comma						|	-- ,"
	Dot							|	-- .
	Concatenate					|	-- ..
	VarArg							-- ...

	deriving (Show)
