module GLua.AST where

import GLua.TokenTypes

data AST = AST deriving (Show)

data FuncName = FuncName [MToken] (Maybe MToken) deriving (Show)-- name(.name)*(:name)?

data Expr = ANil | AFalse | ATrue | ANumber String | AString MToken | AVarArg | AFunctionDef {- todo -} | APrefixExpr {- todo -} | ATableConstructor {- todo -}
            | BinOpExpr Expr BinOp Expr
            | UnOpExpr UnOp Expr deriving (Show)

data Args = ListArgs [Expr] | TableArg [Field] | StringArg MToken deriving (Show)

data Field = ExprField Expr Expr | NamedField MToken Expr | UnnamedField Expr deriving (Show)

data BinOp = APlus | BinMinus | AMultiply | ADivide | AModulus | APower | AConcatenate |
             ALT | ALEQ | AGT | AGEQ | AEq | ANEq | AAnd | AOr deriving (Show)

data UnOp = UnMinus | ANot | AHash deriving (Show)
