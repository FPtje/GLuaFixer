-- | Contains class instances and functions related to tokens
module GLua.TokenTypes where

import Data.List
import Text.ParserCombinators.UU.BasicInstances
import GLua.AG.Token


instance Show MToken where
    show (MToken _ tok) = show tok

-- | Simple EQ instance. TODO: check for position equality
instance Eq MToken where
    (MToken _ t1) == (MToken _ t2) = t1 == t2

-- | Simple Ord instance. TODO: check for position Ord
instance Ord MToken where
    compare (MToken _ t1) (MToken _ t2) = compare t1 t2

-- | Metatoken algebra
type MTokenAlgebra mtok = LineColPos -> Token -> mtok

mpos :: MToken -> LineColPos
mpos (MToken p _) = p

mtok :: MToken -> Token
mtok (MToken _ t) = t

-- | Fold over metatoken
foldMToken :: MTokenAlgebra t -> MToken -> t
foldMToken alg (MToken pos t) = alg pos t

-- | mFold: Apply a TokenAlgebra to an MToken
mFold :: TokenAlgebra a -> MToken -> a
mFold alg mt = foldMToken f mt
  where
    f _ t = foldToken alg t

-- | Huge token algebra
type TokenAlgebra token = (
    ( -- Comments and whitespace
        String -> token,
        String -> token,    -- DashComment
        Int -> String -> token,    -- DashBlockComment
        String -> token,    -- SlashComment
        String -> token,    -- SlashBlockComment
        token               -- Semicolon
    ),
    ( -- Constants
        String -> token,    -- TNumber
        String -> token,    -- DQString
        String -> token,    -- SQString
        String -> token,    -- MLString
        token,              -- TTrue
        token,              -- TFalse
        token,              -- Nil
        token               -- VarArg
    ), -- operators
    (
        token,              -- Plus
        token,              -- Minus
        token,              -- Multiply
        token,              -- Divide
        token,              -- Modulus
        token,              -- Power
        token,              -- TEq
        token,              -- TNEq
        token,              -- TCNEq
        token,              -- TLEQ
        token,              -- TGEQ
        token,              -- TLT
        token,              -- TGT
        token,              -- Equals
        token,              -- Concatenate
        token,              -- Colon
        token,              -- Dot
        token,              -- Comma
        token,              -- Hash
        token,              -- Not
        token,              -- CNot
        token,              -- And
        token,              -- CAnd
        token,              -- Or
        token               -- COr
    ),
    ( -- Keywords
        token,              -- Function
        token,              -- Local
        token,              -- If
        token,              -- Then
        token,              -- Elseif
        token,              -- Else
        token,              -- For
        token,              -- In
        token,              -- Do
        token,              -- While
        token,              -- Until
        token,              -- Repeat
        token,              -- Continue
        token,              -- Break
        token,              -- Return
        token,              -- End
        token               -- Goto
    ),
    ( -- Brackets
        token,              -- LRound
        token,              -- RRound
        token,              -- LCurly
        token,              -- RCurly
        token,              -- LSquare
        token               -- RSquare
    ),
    ( -- Other
        String -> token,    -- Label
        String -> token     -- Identifier
    )
    )

-- | Fold over token definition
foldToken :: TokenAlgebra t -> Token -> t
foldToken ((tWhitespace, tDashComment, tDashBlockComment, tSlashComment, tSlashBlockComment, tSemicolon), (tTNumber, tDQString, tSQString, tMLString, tTTrue, tTFalse, tNil, tVarArg), (tPlus, tMinus, tMultiply, tDivide, tModulus, tPower, tTEq, tTNEq, tTCNEq, tTLEQ, tTGEQ, tTLT, tTGT, tEquals, tConcatenate, tColon, tDot, tComma, tHash, tNot, tCNot, tAnd, tCAnd, tOr, tCOr), (tFunction, tLocal, tIf, tThen, tElseif, tElse, tFor, tIn, tDo, tWhile, tUntil, tRepeat, tContinue, tBreak, tReturn, tEnd, tGoto), (tLRound, tRRound, tLCurly, tRCurly, tLSquare, tRSquare), (tLabel, tIdentifier)) = fold
    where
        fold (Whitespace str) = tWhitespace str
        fold (DashComment str) = tDashComment str
        fold (DashBlockComment depth str) = tDashBlockComment depth str
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
        fold Continue = tContinue
        fold Return = tReturn
        fold Then = tThen
        fold TTrue = tTTrue
        fold Until = tUntil
        fold While = tWhile
        fold Plus = tPlus
        fold Minus = tMinus
        fold Multiply = tMultiply
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

-- | Simple show instance
instance Show Token where
    show = foldToken ((
        id,
        \s -> "--" ++ s, -- DashComment
        \d s -> let n = replicate d '=' in "--[" ++ n ++ '[' : s ++ ']' : n ++ "]", -- DashBlockComment
        \s -> "//" ++ s, -- SlashComment
        \s -> "/*" ++ s ++ "*/", -- SlashBlockComment
        ";" -- Semicolon
        ),
        (
        id, -- TNumber
        \s -> "\"" ++ s ++ "\"", -- DQString
        \s -> "'" ++ s ++ "'", -- SQString
        id, -- MLString
        "true", -- TTrue
        "false", -- TFalse
        "nil", -- Nil
        "..." -- VarArg
        ),
        (
        "+", -- Plus
        "-", -- Minus
        "*", -- Multiply
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
        "continue", -- Continue
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
        \s -> "::" ++ s ++ "::", -- Label
        id -- Identifier
        )
        )

-- | Whether an mtoken is a comment
isWhitespace :: MToken -> Bool
isWhitespace = mFold ((const True,const False,\_ _ -> False,const False,const False,False),(const False,const False,const False,const False,False,False,False,False),(False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False),(False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False),(False,False,False,False,False,False),(const False,const False))

-- | Whether an mtoken is a comment
isComment :: MToken -> Bool
isComment = mFold ((const False,const True,\_ _ -> True,const True,const True,False),(const False,const False,const False,const False,False,False,False,False),(False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False),(False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False),(False,False,False,False,False,False),(const False,const False))

-- | Split the tokens by comments and other tokens
splitComments :: [MToken] -> ([MToken], [MToken])
splitComments = partition isComment

tokenLabel :: MToken -> String
tokenLabel = mFold ((const "", const "",\_ _ -> "",const "",const "",""),(const "",const "",const "",const "","","","",""),("","","","","","","","","","","","","","","","","","","","","","","","",""),("","","","","","","","","","","","","","","","",""),("","","","","",""),(id, id))

-- | The size of a token in characters
tokenSize :: Token -> Int
tokenSize = foldToken ((
    length, -- whitespace
    (+2) . length, -- DashComment
    \d s -> 6 + length s + 2 * d, -- DashBlockComment
    (+2) . length, -- SlashComment
    (+4) . length, -- SlashBlockComment
    1 -- Semicolon
    ),
    (
    length, -- TNumber
    (+2) . length, -- DQString
    (+2) . length, -- SQString
    length, -- MLString
    4, -- TTrue
    5, -- TFalse
    3, -- Nil
    3 -- VarArg
    ),
    (
    1, -- Plus
    1, -- Minus
    1, -- Multiply
    1, -- Divide
    1, -- Modulus
    1, -- Power
    2, -- TEq
    2, -- TNEq
    2, -- TCNEq
    2, -- TLEQ
    2, -- TGEQ
    1, -- TLT
    1, -- TGT
    1, -- Equals
    2, -- Concatenate
    1, -- Colon
    1, -- Dot
    1, -- Comma
    1, -- Hash
    3, -- Not
    1, -- CNot
    3, -- And
    2, -- CAnd
    2, -- Or
    2 -- COr
    ),
    (
    8, -- Function
    5, -- Local
    2, -- If
    4, -- Then
    6, -- Elseif
    4, -- Else
    3, -- For
    2, -- In
    2, -- Do
    5, -- While
    5, -- Until
    6, -- Repeat
    8, -- Continue
    5, -- Break
    6, -- Return
    3, -- End
    4 -- Goto
    ),
    (
    1, -- LRound
    1, -- RRound
    1, -- LCurly
    1, -- RCurly
    1, -- LSquare
    1 -- RSquare
    ),
    (
    (+2) . length, -- Label
    length -- Identifier
    )
    )
