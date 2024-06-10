{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Contains class instances and functions related to tokens
module GLua.TokenTypes where

import Data.Aeson.TH (deriveJSON)
import Data.List (foldl', partition)
import GLua.AG.Token (MToken (..), Token (..))
import GLua.EncodingOptions (encodingOptions)
import GLua.Position (LineColPos (..), Region (..))

instance Show MToken where
  show (MToken _ tok) = show tok

-- | Simple EQ instance. TODO: check for position equality
instance Eq MToken where
  (MToken _ t1) == (MToken _ t2) = t1 == t2

-- | Simple Ord instance. TODO: check for position Ord
instance Ord MToken where
  compare (MToken _ t1) (MToken _ t2) = compare t1 t2

mpos :: MToken -> Region
mpos (MToken p _) = p

mtok :: MToken -> Token
mtok (MToken _ t) = t

-- | Map function for an MToken
mapMtok :: (Token -> a) -> MToken -> a
mapMtok f (MToken _ t) = f t

----------------------------------------
--  Correcting token positions
----------------------------------------
customAdvanceChr :: LineColPos -> Char -> LineColPos
customAdvanceChr (LineColPos line _ abs') '\n' = LineColPos (line + 1) 0 (abs' + 1)
customAdvanceChr (LineColPos line pos' abs') _ = LineColPos line (pos' + 1) (abs' + 1)

customAdvanceStr :: LineColPos -> String -> LineColPos
customAdvanceStr = foldl' customAdvanceChr

customAdvanceToken :: LineColPos -> Token -> LineColPos
customAdvanceToken (LineColPos line pos' abs') t = let len = tokenSize t in LineColPos line (pos' + len) (abs' + len)

-- | Simple show instance
instance Show Token where
  show token = case token of
    Whitespace s -> s
    DashComment c -> "--" ++ c
    DashBlockComment d s -> let n = replicate d '=' in "--[" ++ n ++ '[' : s ++ ']' : n ++ "]"
    SlashComment c -> "//" ++ c
    SlashBlockComment s -> "/*" ++ s ++ "*/"
    Semicolon -> ";"
    TNumber n -> n
    DQString s -> "\"" ++ s ++ "\""
    SQString s -> "'" ++ s ++ "'"
    MLString s -> s
    TTrue -> "true"
    TFalse -> "false"
    Nil -> "nil"
    VarArg -> "..."
    Plus -> "+"
    Minus -> "-"
    Multiply -> "*"
    Divide -> "/"
    Modulus -> "%"
    Power -> "^"
    TEq -> "=="
    TNEq -> "~="
    TCNEq -> "!="
    TLEQ -> "<="
    TGEQ -> ">="
    TLT -> "<"
    TGT -> ">"
    Equals -> "="
    Concatenate -> ".."
    Colon -> ":"
    Dot -> "."
    Comma -> ","
    Hash -> "#"
    Not -> "not"
    CNot -> "!"
    And -> "and"
    CAnd -> "&&"
    Or -> "or"
    COr -> "||"
    Function -> "function"
    Local -> "local"
    If -> "if"
    Then -> "then"
    Elseif -> "elseif"
    Else -> "else"
    For -> "for"
    In -> "in"
    Do -> "do"
    While -> "while"
    Until -> "until"
    Repeat -> "repeat"
    Continue -> "continue"
    Break -> "break"
    Return -> "return"
    End -> "end"
    LRound -> "("
    RRound -> ")"
    LCurly -> "{"
    RCurly -> "}"
    LSquare -> "["
    RSquare -> "]"
    Label spaceBefore ident spaceAfter -> spaceBefore ++ ident ++ spaceAfter
    Identifier i -> i

-- | Whether an mtoken is a comment
isWhitespace :: MToken -> Bool
isWhitespace = mapMtok $ \case
  Whitespace{} -> True
  _ -> False

-- | Whether an mtoken is a comment
isComment :: MToken -> Bool
isComment = mapMtok $ \case
  DashComment{} -> True
  DashBlockComment{} -> True
  SlashComment{} -> True
  SlashBlockComment{} -> True
  _ -> False

-- | Split the tokens by comments and other tokens
splitComments :: [MToken] -> ([MToken], [MToken])
splitComments = partition isComment

-- | Extracts the label name out of the token, returns empty string when the token is not a label or
-- identifier.
tokenLabel :: MToken -> String
tokenLabel = mapMtok $ \case
  Label _ ident _ -> ident
  Identifier ident -> ident
  _ -> ""

-- | The size of a token in characters
tokenSize :: Token -> Int
tokenSize = \case
  Whitespace s -> length s
  DashComment c -> 2 + length c
  DashBlockComment d s -> 6 + length s + 2 * d
  SlashComment c -> length c + 2
  SlashBlockComment s -> length s + 2
  Semicolon -> 1
  TNumber n -> length n
  DQString s -> length s + 2
  SQString s -> length s + 2
  MLString s -> length s
  TTrue -> 4
  TFalse -> 5
  Nil -> 3
  VarArg -> 3
  Plus -> 1
  Minus -> 1
  Multiply -> 1
  Divide -> 1
  Modulus -> 1
  Power -> 1
  TEq -> 2
  TNEq -> 2
  TCNEq -> 2
  TLEQ -> 2
  TGEQ -> 2
  TLT -> 1
  TGT -> 1
  Equals -> 1
  Concatenate -> 2
  Colon -> 1
  Dot -> 1
  Comma -> 1
  Hash -> 1
  Not -> 3
  CNot -> 1
  And -> 3
  CAnd -> 2
  Or -> 2
  COr -> 2
  Function -> 8
  Local -> 5
  If -> 2
  Then -> 4
  Elseif -> 6
  Else -> 4
  For -> 3
  In -> 2
  Do -> 2
  While -> 5
  Until -> 5
  Repeat -> 6
  Continue -> 8
  Break -> 5
  Return -> 6
  End -> 3
  LRound -> 1
  RRound -> 1
  LCurly -> 1
  RCurly -> 1
  LSquare -> 1
  RSquare -> 1
  Label spaceBefore ident spaceAfter -> 2 + length spaceBefore + length ident + length spaceAfter + 2
  Identifier i -> length i

$(deriveJSON encodingOptions ''Token)
$(deriveJSON encodingOptions ''MToken)
