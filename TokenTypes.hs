module TokenTypes where

data Token =
	-- Comments and whitespace
	Whitespace String			|
	DashComment String			|
	DashBlockComment String		|
	SlashComment String			|
	SlashBlockComment String	|
	Semicolon					|	-- ;

	-- Constants
	TNumber String				|
	DQString String				|	-- Double quote string
	SQString String				|	-- Single quote string
	MLString String				|	-- Multiline string
	TTrue						|
	TFalse						|
	Nil							|
	VarArg						|	-- ...

	-- Operators
	Plus						|	-- +
	Minus						|	-- -
	Mulitply					|	-- *
	Divide						|	-- /
	Modulus						|	-- %
	Power						|	-- ^
	TEq							|	-- ==
	TNEq						|	-- ~=
	TCNEq						|	-- !=
	TLEQ						|	-- <=
	TGEQ						|	-- >=
	TLT							|	-- <
	TGT							|	-- >
	Equals						|	-- =
	Concatenate					|	-- ..
	Colon						|	-- :
	Dot							|	-- .
	Comma						|	-- ,
	Hash						|	-- #
	Not							|
	CNot						|	-- !
	And							|
	CAnd						|
	Or							|
	COr							|

	-- Keywords
	Function					|
	Local						|
	If							|
	Then						|
	Elseif						|
	Else						|
	For							|
	In							|
	Do							|
	While						|
	Until						|
	Repeat						|
	Break						|
	Return						|
	End							|
	Goto						|

	LRound						|	-- (
	RRound						|	-- )
	LCurly						|	-- {
	RCurly						|	-- }
	LSquare						|	-- [
	RSquare 					|	-- ]
	Label String				|	-- ::
	Identifier String


type TokenAlgebra token = (
	( -- Comments and whitespace
		String -> token,	-- Whitespace
		String -> token,	-- DashComment
		String -> token,	-- DashBlockComment
		String -> token,	-- SlashComment
		String -> token,	-- SlashBlockComment
		token				-- Semicolon
	),
	( -- Constants
		String -> token,	-- TNumber
		String -> token,	-- DQString
		String -> token,	-- SQString
		String -> token,	-- MLString
		token,				-- TTrue
		token,				-- TFalse
		token,				-- Nil
		token				-- VarArg
	), -- operators
	(
		token,				-- Plus
		token,				-- Minus
		token,				-- Mulitply
		token,				-- Divide
		token,				-- Modulus
		token,				-- Power
		token,				-- TEq
		token,				-- TNEq
		token,				-- TCNEq
		token,				-- TLEQ
		token,				-- TGEQ
		token,				-- TLT
		token,				-- TGT
		token,				-- Equals
		token,				-- Concatenate
		token,				-- Colon
		token,				-- Dot
		token,				-- Comma
		token,				-- Hash
		token,				-- Not
		token,				-- CNot
		token,				-- And
		token,				-- CAnd
		token,				-- Or
		token				-- COr
	),
	( -- Keywords
		token,				-- Function
		token,				-- Local
		token,				-- If
		token,				-- Then
		token,				-- Elseif
		token,				-- Else
		token,				-- For
		token,				-- In
		token,				-- Do
		token,				-- While
		token,				-- Until
		token,				-- Repeat
		token,				-- Break
		token,				-- Return
		token,				-- End
		token				-- Goto
	),
	( -- Brackets
		token,				-- LRound
		token,				-- RRound
		token,				-- LCurly
		token,				-- RCurly
		token,				-- LSquare
		token				-- RSquare
	),
	( -- Other
		String -> token,	-- Label
		String -> token		-- Identifier
	)
	)

foldToken :: TokenAlgebra t -> Token -> t
foldToken ((tWhitespace, tDashComment, tDashBlockComment, tSlashComment, tSlashBlockComment, tSemicolon), (tTNumber, tDQString, tSQString, tMLString, tTTrue, tTFalse, tNil, tVarArg), (tPlus, tMinus, tMulitply, tDivide, tModulus, tPower, tTEq, tTNEq, tTCNEq, tTLEQ, tTGEQ, tTLT, tTGT, tEquals, tConcatenate, tColon, tDot, tComma, tHash, tNot, tCNot, tAnd, tCAnd, tOr, tCOr), (tFunction, tLocal, tIf, tThen, tElseif, tElse, tFor, tIn, tDo, tWhile, tUntil, tRepeat, tBreak, tReturn, tEnd, tGoto), (tLRound, tRRound, tLCurly, tRCurly, tLSquare, tRSquare), (tLabel, tIdentifier)) = fold
	where
		fold (Whitespace str) = tWhitespace str
		fold (DashComment str) = tDashComment str
		fold (DashBlockComment str) = tDashBlockComment str
		fold (SlashComment str) = tSlashComment str
		fold (SlashBlockComment str) = tSlashBlockComment str
		fold (TNumber str) = tTNumber str
		fold (Label str) = tLabel str
		fold (Identifier str) = tIdentifier str
		fold (DQString str) = tDQString str
		fold (SQString str) = tSQString str
		fold (MLString str) = tMLString str
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
		fold CNot = tCNot
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
	show = foldToken ((
		\s -> "{" ++ s ++ "}", -- Whitespace
		id, -- DashComment
		id, -- DashBlockComment
		id, -- SlashComment
		id, -- SlashBlockComment
		";" -- Semicolon
		),
		(
		id, -- TNumber
		id, -- DQString
		id, -- SQString
		id, -- MLString
		"true", -- TTrue
		"false", -- TFalse
		"nil", -- Nil
		"..." -- VarArg
		),
		(
		"+", -- Plus
		"-", -- Minus
		"*", -- Mulitply
		"/", -- Divide
		"%", -- Modulus
		"^", -- Power
		"==", -- TEq
		"~=", -- TNEq
		"!=", -- TCNEq
		"<=", -- TLEQ
		">=", -- TGEQ
		"<", -- TLT
		">", -- TGT
		"=", -- Equals
		"..", -- Concatenate
		":", -- Colon
		".", -- Dot
		",", -- Comma
		"#", -- Hash
		"not", -- Not
		"!", -- CNot
		"and", -- And
		"&&", -- CAnd
		"or", -- Or
		"||" -- COr
		),
		(
		"function", -- Function
		"local", -- Local
		"if", -- If
		"then", -- Then
		"elseif", -- Elseif
		"else", -- Else
		"for", -- For
		"in", -- In
		"do", -- Do
		"while", -- While
		"until", -- Until
		"repeat", -- Repeat
		"break", -- Break
		"return", -- Return
		"end", -- End
		"goto" -- Goto
		),
		(
		"(", -- LRound
		")", -- RRound
		"{", -- LCurly
		"}", -- RCurly
		"[", -- LSquare
		"]" -- RSquare
		),
		(
		\s -> "::" ++ s, -- Label
		id -- Identifier
		)
		)

isWhitespace :: Token -> Bool
isWhitespace = foldToken ((const True,const False,const False,const False,const False,False),( const False,const False,const False,const False,False,False,False,False),(False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False),(False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False),(False,False,False,False,False,False),(const False,const False))


