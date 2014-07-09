module TokenTypes where

data Token =
	Whitespace String			|
	DashComment String			|
	DashBlockComment String		|
	SlashComment String			|
	SlashBlockComment String	|

	TNumber String				|
	Label String				|	-- ::
	Identifier String 			|

	-- Keywords
	And							|
	CAnd						|
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
	COr							|
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
	TCNEq						|	-- !=
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
	Semicolon					|	-- ;
	Colon						|	-- :
	Comma						|	-- ,"
	Dot							|	-- .
	Concatenate					|	-- ..
	VarArg							-- ...


type TokenAlgebra token = (
	String -> token,	-- Whitespace
	String -> token,	-- DashComment
	String -> token,	-- DashBlockComment
	String -> token,	-- SlashComment
	String -> token,	-- SlashBlockComment
	String -> token,	-- TNumber
	String -> token,	-- Label
	String -> token,	-- Identifier

	token,				-- And
	token,				-- CAnd
	token,				-- Break
	token,				-- Do
	token,				-- Else
	token,				-- Elseif
	token,				-- End
	token,				-- TFalse
	token,				-- For
	token,				-- Function
	token,				-- Goto
	token,				-- If
	token,				-- In
	token,				-- Local
	token,				-- Nil
	token,				-- Not
	token,				-- Or
	token,				-- COr
	token,				-- Repeat
	token,				-- Return
	token,				-- Then
	token,				-- TTrue
	token,				-- Until
	token,				-- While
	token,				-- Plus
	token,				-- Minus
	token,				-- Mulitply
	token,				-- Divide
	token,				-- Modulus
	token,				-- Power
	token,				-- Hash
	token,				-- TEq
	token,				-- TNEq
	token,				-- TCNEq
	token,				-- TLEQ
	token,				-- TGEQ
	token,				-- TLT
	token,				-- TGT
	token,				-- Equals
	token,				-- LRound
	token,				-- RRound
	token,				-- LCurly
	token,				-- RCurly
	token,				-- LSquare
	token,				-- RSquare
	token,				-- Semicolon
	token,				-- Colon
	token,				-- Comma
	token,				-- Dot
	token,				-- Concatenate
	token				-- VarArg
	)

foldToken :: TokenAlgebra t -> Token -> t
foldToken (tWhitespace, tDashComment, tDashBlockComment, tSlashComment, tSlashBlockComment, tTNumber, tLabel, tIdentifier, tAnd, tCAnd, tBreak, tDo, tElse, tElseif, tEnd, tTFalse, tFor, tFunction, tGoto, tIf, tIn, tLocal, tNil, tNot, tOr, tCOr, tRepeat, tReturn, tThen, tTTrue, tUntil, tWhile, tPlus, tMinus, tMulitply, tDivide, tModulus, tPower, tHash, tTEq, tTNEq, tTCNEq, tTLEQ, tTGEQ, tTLT, tTGT, tEquals, tLRound, tRRound, tLCurly, tRCurly, tLSquare, tRSquare, tSemicolon, tColon, tComma, tDot, tConcatenate, tVarArg) = fold
	where
		fold (Whitespace str) = tWhitespace str
		fold (DashComment str) = tDashComment str
		fold (DashBlockComment str) = tDashBlockComment str
		fold (SlashComment str) = tSlashComment str
		fold (SlashBlockComment str) = tSlashBlockComment str
		fold (TNumber str) = tTNumber str
		fold (Label str) = tLabel str
		fold (Identifier str) = tIdentifier str
		fold And = tAnd
		fold CAnd = tCAnd
		fold Break = tBreak
		fold Do = tDo
		fold Else = tElse
		fold Elseif = tElseif
		fold End = tEnd
		fold TFalse = tTFalse
		fold For = tFor
		fold Function = tFunction
		fold Goto = tGoto
		fold If = tIf
		fold In = tIn
		fold Local = tLocal
		fold Nil = tNil
		fold Not = tNot
		fold Or = tOr
		fold COr = tCOr
		fold Repeat = tRepeat
		fold Return = tReturn
		fold Then = tThen
		fold TTrue = tTTrue
		fold Until = tUntil
		fold While = tWhile
		fold Plus = tPlus
		fold Minus = tMinus
		fold Mulitply = tMulitply
		fold Divide = tDivide
		fold Modulus = tModulus
		fold Power = tPower
		fold Hash = tHash
		fold TEq = tTEq
		fold TNEq = tTNEq
		fold TCNEq = tTCNEq
		fold TLEQ = tTLEQ
		fold TGEQ = tTGEQ
		fold TLT = tTLT
		fold TGT = tTGT
		fold Equals = tEquals
		fold LRound = tLRound
		fold RRound = tRRound
		fold LCurly = tLCurly
		fold RCurly = tRCurly
		fold LSquare = tLSquare
		fold RSquare = tRSquare
		fold Semicolon = tSemicolon
		fold Colon = tColon
		fold Comma = tComma
		fold Dot = tDot
		fold Concatenate = tConcatenate
		fold VarArg = tVarArg

instance Show Token where
	show = foldToken (
		id,
		id,
		id,
		id,
		id,
		id,
		id,
		id,
		"and",
		"&&",
		"break",
		"do",
		"else",
		"elseif",
		"end",
		"false",
		"for",
		"function",
		"goto",
		"if",
		"in",
		"local",
		"nil",
		"not",
		"or",
		"||",
		"repeat",
		"return",
		"then",
		"true",
		"until",
		"while",
		"+",
		"-",
		"*",
		"/",
		"%",
		"^",
		"#",
		"==",
		"~=",
		"!=",
		"<=",
		">=",
		"<",
		">",
		"=",
		"(",
		")",
		"{",
		"}",
		"[",
		"]",
		";",
		":",
		",",
		".",
		"..",
		"..."
		)

isWhitespace :: Token -> Bool
isWhitespace = foldToken (const True, const False, const False, const False, const False, const False, const False, const False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False)
