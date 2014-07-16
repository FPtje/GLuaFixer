module GLua.AST where

import GLua.TokenTypes

data AST = AST deriving (Show)

data BinOp = APlus | BinMinus | AMultiply | ADivide | AModulus | APower | AConcatenate |
             ALT | ALEQ | AGT | AGEQ | AEq | ANEq | AAnd | AOr deriving (Show)

data UnOp = UnMinus | ANot | AHash deriving (Show)
