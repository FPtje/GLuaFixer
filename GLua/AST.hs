module GLua.AST where

import GLua.TokenTypes

-- Block of code
data Block = Block [Stat] AReturn deriving (Show)

-- Statement
data Stat = ASemicolon |
            Def [(PrefixExp, Expr)] | -- varlist = exprList
            LocDef [(PrefixExp, Expr)] | -- local a,b,c, ... = ...
            AFuncCall PrefixExp | -- Function call
            ALabel MToken | -- label
            ABreak | -- break
            AGoto MToken | -- goto label
            ADo Block | -- do block end
            AWhile Expr Block | -- while expr do block end
            ARepeat Block Expr | -- repeat block until expr
            AIf Expr Block [(Expr, Block)] [Block] | -- if expr then block [elseif expr then block] [else block] end
            ANFor MToken Expr Expr Expr Block | -- for varname = expr, [expr]+ do block end, numeric for
            AGFor [MToken] [Expr] Block | -- for namelist in expr do block end
            AFunc FuncName [MToken] Block | -- function funcname(parameters) block end
            ALocFunc FuncName [MToken] Block -- local function funcname(parameters) block end
            deriving (Show)


-- Return [values]
data AReturn = AReturn [Expr] | NoReturn deriving (Show)

-- Function names
data FuncName = FuncName [MToken] (Maybe MToken) deriving (Show)-- name(.name)*(:name)?

-- Prefix expressions (vars or function calls)
data PrefixExp = PFVar MToken [PFExprSuffix] | ExprVar Expr [PFExprSuffix] deriving (Show)

-- Suffix of expression: Call(), :MetaCall(), [indexation] or .indexation
data PFExprSuffix = Call Args | MetaCall MToken Args | ExprIndex Expr | DotIndex MToken deriving (Show)

-- Expressions
data Expr = ANil |
            AFalse |
            ATrue |
            ANumber String |
            AString MToken |
            AVarArg |
            AnonymousFunc [MToken] Block |
            APrefixExpr PrefixExp |
            ATableConstructor [Field] |
            BinOpExpr BinOp Expr Expr |
            UnOpExpr UnOp Expr
            deriving (Show)

-- arguments of function call: fn(list, args), fn{table, args}, fn"string args"
data Args = ListArgs [Expr] | TableArg [Field] | StringArg MToken deriving (Show)

-- Fields of a table: {[expr] = expr, namedfield = expr, expr}
data Field = ExprField Expr Expr | NamedField MToken Expr | UnnamedField Expr deriving (Show)

-- Binary operators
data BinOp = APlus | BinMinus | AMultiply | ADivide | AModulus | APower | AConcatenate |
             ALT | ALEQ | AGT | AGEQ | AEq | ANEq | AAnd | AOr deriving (Show)

-- Unary operators
data UnOp = UnMinus | ANot | AHash deriving (Show)
