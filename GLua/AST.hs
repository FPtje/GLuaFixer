module GLua.AST where

import GLua.TokenTypes

data AST = AST deriving (Show)

data FuncName = FuncName [MToken] (Maybe MToken) deriving (Show)-- name(.name)*(:name)?

data BinOp = APlus | BinMinus | AMultiply | ADivide | AModulus | APower | AConcatenate |
             ALT | ALEQ | AGT | AGEQ | AEq | ANEq | AAnd | AOr deriving (Show)

data UnOp = UnMinus | ANot | AHash deriving (Show)
