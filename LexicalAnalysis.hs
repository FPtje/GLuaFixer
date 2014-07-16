module LexicalAnalysis where

import GLua.TokenTypes

-- Insert a token between two other tokens based on a condition
insertTokens :: (Token -> Token -> Maybe Token) -> [Token] -> [Token]
insertTokens cond [t] = case cond t (Whitespace "") of
                            Just ins -> [t, ins]
                            Nothing -> [t]
insertTokens cond (left : xs@(right : ts)) = case cond left right of
                            Just between -> left : between : insertTokens cond xs
                            Nothing -> left : insertTokens cond xs

ensureWhiteSpaceAfter :: Token -> [Token] -> [Token]
ensureWhiteSpaceAfter tok = insertTokens placeWhite
    where
        placeWhite :: Token -> Token -> Maybe Token
        placeWhite _ (Whitespace _) = Nothing
        placeWhite t _              | t == tok = Just (Whitespace " ")
                                    | otherwise = Nothing


-- Makes sure a token is a pure Lua token
-- rather than a Garry's mod Lua token (e.g. changes != to ~=, /* */ to --[[]])
-- Note! when converting a list of tokens, make sure there's whitespace after every "not"!
toPureLua :: [Token] -> [Token]
toPureLua = ensureWhiteSpaceAfter Not . map (foldToken ((
    Whitespace, -- Whitespace
    DashComment, -- DashComment
    DashBlockComment, -- DashBlockComment
    DashComment, -- SlashComment to DashComment
    DashBlockComment 0, -- SlashBlockComment
    Semicolon -- Semicolon
    ),
    (
    TNumber, -- TNumber
    DQString, -- DQString
    SQString, -- SQString
    MLString, -- MLString
    TTrue, -- TTrue
    TFalse, -- TFalse
    Nil, -- Nil
    VarArg -- VarArg
    ),
    (
    Plus, -- Plus
    Minus, -- Minus
    Multiply, -- Multiply
    Divide, -- Divide
    Modulus, -- Modulus
    Power, -- Power
    TEq, -- TEq
    TNEq, -- TNEq
    TNEq, -- TCNEq to TNEq (!= to ~=)
    TLEQ, -- TLEQ
    TGEQ, -- TGEQ
    TLT, -- TLT
    TGT, -- TGT
    Equals, -- Equals
    Concatenate, -- Concatenate
    Colon, -- Colon
    Dot, -- Dot
    Comma, -- Comma
    Hash, -- Hash
    Not, -- Not
    Not, -- CNot (! to not)
    And, -- And
    And, -- CAnd to And (&& to and)
    Or, -- Or
    Or  -- COr to or (|| to or)
    ),
    (
    Function, -- Function
    Local, -- Local
    If, -- If
    Then, -- Then
    Elseif, -- Elseif
    Else, -- Else
    For, -- For
    In, -- In
    Do, -- Do
    While, -- While
    Until, -- Until
    Repeat, -- Repeat
    Continue, -- Continue
    Break, -- Break
    Return, -- Return
    End, -- End
    Goto  -- Goto
    ),
    (
    LRound, -- LRound
    RRound, -- RRound
    LCurly, -- LCurly
    RCurly, -- RCurly
    LSquare, -- LSquare
    RSquare  -- RSquare
    ),
    (
    Label, -- Label
    Identifier -- Identifier
    )
    )
    )

-- Makes sure a token is a GLua token where possible (e.g. changes ~= to !=)
toGLua :: [Token] -> [Token]
toGLua = map $ foldToken ((
    Whitespace, -- Whitespace
    SlashComment, -- DashComment to SlashComment
    const SlashBlockComment, -- DashBlockComment to SlashBlockComment
    SlashComment, -- DashComment to SlashComment
    SlashBlockComment, -- SlashBlockComment
    Semicolon -- Semicolon
    ),
    (
    TNumber, -- TNumber
    DQString, -- DQString
    SQString, -- SQString
    MLString, -- MLString
    TTrue, -- TTrue
    TFalse, -- TFalse
    Nil, -- Nil
    VarArg -- VarArg
    ),
    (
    Plus, -- Plus
    Minus, -- Minus
    Multiply, -- Multiply
    Divide, -- Divide
    Modulus, -- Modulus
    Power, -- Power
    TEq, -- TEq
    TCNEq, -- TNEq to TCNEq (~= to !=)
    TCNEq, -- TCEq
    TLEQ, -- TLEQ
    TGEQ, -- TGEQ
    TLT, -- TLT
    TGT, -- TGT
    Equals, -- Equals
    Concatenate, -- Concatenate
    Colon, -- Colon
    Dot, -- Dot
    Comma, -- Comma
    Hash, -- Hash
    CNot, -- Not (not to !)
    CNot, -- CNot
    CAnd, -- And to CAnd
    CAnd, -- CAnd
    COr, -- Or to COr
    COr  -- COr
    ),
    (
    Function, -- Function
    Local, -- Local
    If, -- If
    Then, -- Then
    Elseif, -- Elseif
    Else, -- Else
    For, -- For
    In, -- In
    Do, -- Do
    While, -- While
    Until, -- Until
    Repeat, -- Repeat
    Continue, -- Continue
    Break, -- Break
    Return, -- Return
    End, -- End
    Goto  -- Goto
    ),
    (
    LRound, -- LRound
    RRound, -- RRound
    LCurly, -- LCurly
    RCurly, -- RCurly
    LSquare, -- LSquare
    RSquare  -- RSquare
    ),
    (
    Label, -- Label
    Identifier -- Identifier
    )
    )
