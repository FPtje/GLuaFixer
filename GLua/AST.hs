module GLua.AST where

import Text.ParserCombinators.UU.BasicInstances
import GLua.TokenTypes

-- Root of the Abstract Syntax Tree
data AST = AST {comments :: [MToken], chunk :: Block} deriving (Show)

-- Block of code
data Block = Block [MStat] AReturn deriving (Show)

data MStat = MStat LineColPos Stat deriving (Show)

-- Statement
data Stat = ASemicolon |
            Def [(PrefixExp, MExpr)] | -- varlist = exprList
            LocDef [(PrefixExp, MExpr)] | -- local a,b,c, ... = ...
            AFuncCall PrefixExp | -- Function call
            ALabel MToken | -- label
            ABreak | -- break
            AContinue | -- continue
            AGoto MToken | -- goto label
            ADo Block | -- do block end
            AWhile MExpr Block | -- while expr do block end
            ARepeat Block MExpr | -- repeat block until expr
            AIf MExpr Block [(MExpr, Block)] [Block] | -- if expr then block [elseif expr then block] [else block] end
            ANFor MToken MExpr MExpr MExpr Block | -- for varname = expr, [expr]+ do block end, numeric for
            AGFor [MToken] [MExpr] Block | -- for namelist in expr do block end
            AFunc FuncName [MToken] Block | -- function funcname(parameters) block end
            ALocFunc FuncName [MToken] Block -- local function funcname(parameters) block end
            deriving (Show)


-- Return [values]
data AReturn = AReturn LineColPos [MExpr] | NoReturn deriving (Show)

-- Function names
data FuncName = FuncName [MToken] (Maybe MToken) deriving (Show)-- name(.name)*(:name)?

-- Prefix expressions (vars or function calls)
data PrefixExp = PFVar MToken [PFExprSuffix] | ExprVar MExpr [PFExprSuffix] deriving (Show)

-- Suffix of expression: Call(), :MetaCall(), [indexation] or .indexation
data PFExprSuffix = Call Args | MetaCall MToken Args | ExprIndex MExpr | DotIndex MToken deriving (Show)

-- Expressions
data MExpr = MExpr LineColPos Expr deriving (Show)

data Expr = ANil |
            AFalse |
            ATrue |
            ANumber String |
            AString MToken |
            AVarArg |
            AnonymousFunc [MToken] Block |
            APrefixExpr PrefixExp |
            ATableConstructor [Field] |
            BinOpExpr BinOp MExpr MExpr |
            UnOpExpr UnOp MExpr
            deriving (Show)

-- arguments of function call: fn(list, args), fn{table, args}, fn"string args"
data Args = ListArgs [MExpr] | TableArg [Field] | StringArg MToken deriving (Show)

-- Fields of a table: {[expr] = expr, namedfield = expr, expr}
data Field = ExprField MExpr MExpr | NamedField MToken MExpr | UnnamedField MExpr deriving (Show)

-- Binary operators
data BinOp = APlus | BinMinus | AMultiply | ADivide | AModulus | APower | AConcatenate |
             ALT | ALEQ | AGT | AGEQ | AEq | ANEq | AAnd | AOr deriving (Show)

-- Unary operators
data UnOp = UnMinus | ANot | AHash deriving (Show)
