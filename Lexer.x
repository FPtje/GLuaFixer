{
module Lexer where

import TokenTypes
}

%wrapper "posn" -- Store position info with the tokens


$digit = 0-9
$hexa  = [0-9a-fA-F]
$alpha = [a-zA-Z]
$all   = [.\n\r]


tokens :-
	$white+														{ wrapToken Whitespace }
	"--[" (=)* "[" ($all # \] | \] $all # [=\]])* "]"(=)*"]" 	{ wrapToken DashBlockComment }
	"--" (. # \[) .*											{ wrapToken DashComment }
	"//" .*														{ wrapToken SlashComment }
	-- /* with ((anything except *) or (* but no / after it))* then */
	"/*" ($all # \* | \* $all # \/)* \*+ \/ 					{ wrapToken SlashBlockComment }

	-- Matches any number
	(((0[xX]$hexa+)(\.?$hexa*))|($digit+)(\.?$digit*))[eEpP]?[\+\-]?$digit* { wrapToken TNumber }

	-- Keywords
	"and"														{ wrapToken (const And) }
	"&&"														{ wrapToken (const CAnd) }
	"break"														{ wrapToken (const Break) }
	"do"														{ wrapToken (const Do) }
	"else"														{ wrapToken (const Else) }
	"elseif"													{ wrapToken (const Elseif) }
	"end"														{ wrapToken (const End) }
	"false"														{ wrapToken (const TFalse) }
	"for"														{ wrapToken (const For) }
	"function"													{ wrapToken (const Function) }
	"goto"														{ wrapToken (const Goto) }
	"if"														{ wrapToken (const If) }
	"in"														{ wrapToken (const In) }
	"local"														{ wrapToken (const Local) }
	"nil"														{ wrapToken (const Nil) }
	"not"														{ wrapToken (const Not) }
	"or"														{ wrapToken (const Or) }
	"||"														{ wrapToken (const COr) }
	"repeat"													{ wrapToken (const Repeat) }
	"return"													{ wrapToken (const Return) }
	"then"														{ wrapToken (const Then) }
	"true"														{ wrapToken (const TTrue) }
	"until"														{ wrapToken (const Until) }
	"while"														{ wrapToken (const While) }


	-- operators
	"+"															{ wrapToken (const Plus) }
	"-"															{ wrapToken (const Minus) }
	"*"															{ wrapToken (const Mulitply) }
	"/"															{ wrapToken (const Divide) }
	"%"															{ wrapToken (const Modulus) }
	"^"															{ wrapToken (const Power) }
	"#"															{ wrapToken (const Hash) }
	"=="														{ wrapToken (const TEq) }
	"~="														{ wrapToken (const TNEq) }
	"!="														{ wrapToken (const TCNEq) }
	"<="														{ wrapToken (const TLEQ) }
	">="														{ wrapToken (const TGEQ) }
	"<"															{ wrapToken (const TLT) }
	">"															{ wrapToken (const TGT) }
	"="															{ wrapToken (const Equals) }
	"("															{ wrapToken (const LRound) }
	")"															{ wrapToken (const RRound) }
	"{"															{ wrapToken (const LCurly) }
	"}"															{ wrapToken (const RCurly) }
	"["															{ wrapToken (const LSquare) }
	"]"															{ wrapToken (const RSquare) }
	"::"														{ wrapToken Label }
	";"															{ wrapToken (const Semicolon) }
	":"															{ wrapToken (const Colon) }
	","															{ wrapToken (const Comma) }
	"."															{ wrapToken (const Dot) }
	".."														{ wrapToken (const Concatenate) }
	"..."														{ wrapToken (const VarArg) }
{

data MToken = MToken AlexPosn Token

instance Show MToken where
	show (MToken (AlexPn _ line col) token) = "[" ++ (show line) ++ ":" ++ (show col) ++ "](" ++ (show token) ++ ")"

wrapToken :: (String -> Token) -> AlexPosn -> String -> MToken
wrapToken token pos str =  MToken pos (token str)

}
