{
module Lexer where

import TokenTypes
}

%wrapper "posn" -- Store position info with the tokens


$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
	$white+									{ wrapToken Whitespace }
	"--" .*                                 { wrapToken DashComment }
	"//" .*                                 { wrapToken SlashComment }

{

data MToken = MToken AlexPosn Token deriving (Show)

wrapToken :: (String -> Token) -> AlexPosn -> String -> MToken
wrapToken token pos str =  MToken pos (token str)

}
