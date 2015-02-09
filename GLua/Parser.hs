{-# LANGUAGE FlexibleInstances,
            TypeSynonymInstances,
            MultiParamTypeClasses,
            Rank2Types, FlexibleContexts, NoMonomorphismRestriction,
            CPP #-}

-- Parser based on www.lua.org/manual/5.2/manual.html#9
module GLua.Parser where

import GLua.TokenTypes
import GLua.AST
import qualified GLua.Lexer as Lex

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived
import qualified Data.ListLike as LL

-- Custom parser that parses MTokens
type AParser a = (IsLocationUpdatedBy LineColPos MToken, LL.ListLike state MToken) => P (Str MToken state LineColPos) a

-- LineColPos is a location that can be updated by MTokens
instance IsLocationUpdatedBy LineColPos MToken where
    -- advance :: LineColPos -> MToken -> LineColPos
    -- Assume the position of the next MToken
    advance pos (MToken p t) = p


-- parse a string directly
parseFromString :: AParser a -> String -> (a, [Error LineColPos])
parseFromString p = execAParser p . fst . Lex.execParseTokens

-- Text.ParserCombinators.UU.Utils.execParser modified to parse MTokens
execAParser :: AParser a -> [MToken] -> (a, [Error LineColPos])
execAParser p = parse_h ((,) <$> p <*> pEnd) . createStr (LineColPos 0 0 0)

-- Parse a single Metatoken, based on a positionless token (much like pSym)
pMTok :: Token -> AParser MToken
pMTok t = pSatisfy isToken (Insertion ("Token " ++ show t) (MToken (LineColPos 0 0 0) t) 5)
    where
        isToken :: MToken -> Bool
        isToken (MToken _ tok) = t == tok

-- Parse an identifier
pName :: AParser MToken
pName = pSatisfy isName (Insertion "Identifier" (MToken (LineColPos 0 0 0) (Identifier "someVariable")) 5)
    where
        isName :: MToken -> Bool
        isName (MToken _ (Identifier _)) = True
        isName _ = False

-- Parse a namelist
parseNameList :: AParser [MToken]
parseNameList = (:) <$> pName <*> pMany (pMTok Comma *> pName)

-- Parse list of parameters
parseParList :: AParser [MToken]
parseParList = pName <**> (
                    pMTok Comma <**> (
                        (\a _ c -> [c, a]) <$> pMTok VarArg <<|>
                        (\a _ c -> c : a)  <$> parseParList
                    ) <<|>
                    pReturn (: [])
               ) <<|> pReturn []

-- Parse a block with an optional return value
parseBlock :: AParser Block
parseBlock = Block <$> pMany parseStat <*> (parseReturn <<|> pReturn NoReturn)

-- Parse a single statement
parseStat :: AParser Stat
parseStat = ASemicolon <$ pMTok Semicolon <<|>
            (AFuncCall <$> pFunctionCall <|>
            -- Function calls and definitions both start with a var
            (\v e -> Def (zip v $ e ++ repeat ANil)) <$> parseVarList <* pMTok Equals <*> parseExpressionList) <<|>
            ALabel <$> parseLabel <<|>
            ABreak <$ pMTok Break <<|>
            AContinue <$ pMTok Continue <<|>
            AGoto <$ pMTok Goto <*> pName <<|>
            ADo <$ pMTok Do <*> parseBlock <* pMTok End <<|>
            AWhile <$ pMTok While <*> parseExpression <* pMTok Do <*> parseBlock <* pMTok End <<|>
            ARepeat <$ pMTok Repeat <*> parseBlock <* pMTok Until <*> parseExpression <<|>
            parseIf <<|>
            parseFor <<|>
            AFunc <$ pMTok Function <*> parseFuncName <*>
             pPacked (pMTok LRound) (pMTok RRound) parseParList <*>
             parseBlock <*
             pMTok End <<|>
            -- local function and local vars both begin with "local"
            pMTok Local <**> (
              -- local function
              (\n p b l -> ALocFunc n p b) <$
                pMTok Function <*> parseFuncName <*> pPacked (pMTok LRound) (pMTok RRound) parseParList <*>
                parseBlock <* pMTok End <<|>
              -- local variables
              (\v e l -> LocDef (zip v $ e ++ repeat ANil)) <$> parseVarList <*> (pMTok Equals *> parseExpressionList <<|> pReturn [])
            )


-- Parse if then elseif then else end expressions
parseIf :: AParser Stat
parseIf = AIf <$ pMTok If <*> parseExpression <* pMTok Then <*>
            parseBlock <*>
            -- elseif
            (pMany ((,) <$ pMTok Elseif <*> parseExpression <* pMTok Then <*> parseBlock)) <*>
            -- else
            (pMany (pMTok Else *> parseBlock)) <*
            pMTok End

-- Parse numeric and generic for loops
parseFor :: AParser Stat
parseFor = ANFor <$ pMTok For <*>
            pName <* pMTok Equals <*> parseExpression <*
            -- end value
            pMTok Comma <*> parseExpression <*>
            -- step (1 if not filled in)
            (pMTok Comma *> parseExpression <<|> pReturn (ANumber "1")) <*
            pMTok Do <*> parseBlock <*
            pMTok End
            <|>
           AGFor <$ pMTok For <*> parseNameList <*
            pMTok In <*> parseExpressionList <*
            pMTok Do <*> parseBlock <*
            pMTok End


parseReturn :: AParser AReturn
parseReturn = AReturn <$ pMTok Return <*> opt parseExpressionList []

-- Label
parseLabel :: AParser MToken
parseLabel = pSatisfy isLabel (Insertion "Label" (MToken (LineColPos 0 0 0) (Label "someLabel")) 5)
    where
        isLabel :: MToken -> Bool
        isLabel (MToken _ (Label _)) = True
        isLabel _ = False

-- function name
parseFuncName :: AParser FuncName
parseFuncName = (\a b c -> FuncName (a:b) c) <$> pName <*> pMany (pMTok Dot *> pName) <*>
                opt (Just <$ pMTok Colon <*> pName) Nothing

parseNumber :: AParser Expr
parseNumber = (\(MToken _ (TNumber str)) -> ANumber str) <$> pSatisfy isNumber (Insertion "Number" (MToken (LineColPos 0 0 0) (TNumber "0")) 5)
    where
        isNumber :: MToken -> Bool
        isNumber (MToken _ (TNumber str)) = True
        isNumber _ = False

parseString :: AParser MToken
parseString = pSatisfy isString (Insertion "String" (MToken (LineColPos 0 0 0) (DQString "someString")) 5)
    where
        isString :: MToken -> Bool
        isString (MToken _ (DQString str)) = True
        isString (MToken _ (SQString str)) = True
        isString (MToken _ (MLString str)) = True
        isString _ = False

-- Parse variable list (var1, var2, var3)
parseVarList :: AParser [PrefixExp]
parseVarList = pList1Sep (pMTok Comma) parseVar

-- single variable. Note: definition differs from reference to circumvent the left recursion
-- var ::= Name [{PFExprSuffix}* indexation] | '(' exp ')' {PFExprSuffix}* indexation
-- where "{PFExprSuffix}* indexation" is any arbitrary sequence of prefix expression suffixes that end with an indexation
parseVar :: AParser PrefixExp
parseVar = PFVar <$> pName <*> (reverse <$> opt suffixes []) <<|>
           ExprVar <$ pMTok LRound <*> parseExpression <* pMTok RRound <*> (reverse <$> suffixes)
    where
        suffixes = (:) <$> pPFExprSuffix <*> suffixes <|>
                   (flip (:) [] . ExprIndex) <$ pMTok LSquare <*> parseExpression <* pMTok RSquare <|>
                   (flip (:) [] . DotIndex) <$ pMTok Dot <*> pName

-- list of expressions
parseExpressionList :: AParser [Expr]
parseExpressionList = parseExpression <**> (
                          (\_ list exp -> exp : list) <$> pMTok Comma <*> parseExpressionList <<|>
                          flip (:) <$> pReturn []
                      )

-- Subexpressions, i.e. without operators
parseSubExpression :: AParser Expr
parseSubExpression = ANil <$ pMTok Nil <<|>
                  AFalse <$ pMTok TFalse <<|>
                  ATrue <$ pMTok TTrue <<|>
                  parseNumber <<|>
                  AString <$> parseString <<|>
                  AVarArg <$ pMTok VarArg <<|>
                  parseAnonymFunc <<|>
                  APrefixExpr <$> parsePrefixExp <<|>
                  ATableConstructor <$> parseTableConstructor

parseAnonymFunc :: AParser Expr
parseAnonymFunc = AnonymousFunc <$
                   pMTok Function <*>
                   pPacked (pMTok LRound) (pMTok RRound) parseParList <*>
                   parseBlock <*
                   pMTok End


-- Parse operators of the same precedence in a chain
samePrio :: [(Token, BinOp)] -> AParser Expr -> AParser Expr
samePrio ops p = pChainl (choice (map f ops)) p
  where
    choice = foldr (<<|>) pFail
    f :: (Token, BinOp) -> AParser (Expr -> Expr -> Expr)
    f (t, at) = BinOpExpr at <$ pMTok t

-- Operators, sorted by priority
-- Priority from: http://www.lua.org/manual/5.2/manual.html#3.4.7
lvl1, lvl2, lvl3, lvl4, lvl5, lvl6, lvl8 :: [(Token, BinOp)]
lvl1 = [(Or, AOr), (COr, AOr)]
lvl2 = [(And, AAnd), (CAnd, AAnd)]
lvl3 = [(TLT, ALT), (TGT, AGT), (TLEQ, ALEQ), (TGEQ, AGEQ), (TNEq, ANEq), (TCNEq, ANEq), (TEq, AEq)]
lvl4 = [(Concatenate, AConcatenate)]
lvl5 = [(Plus, APlus), (Minus, BinMinus)]
lvl6 = [(Multiply, AMultiply), (Divide, ADivide), (Modulus, AModulus)]
-- lvl7 is unary operators
lvl8 = [(Power, APower)]

-- Parse chains of binary and unary operators
parseExpression :: AParser Expr
parseExpression = samePrio lvl1 $
                  samePrio lvl2 $
                  samePrio lvl3 $
                  samePrio lvl4 $
                  samePrio lvl5 $
                  samePrio lvl6 $
                  UnOpExpr <$> parseUnOp <*> parseExpression <<|> -- lvl7
                  samePrio lvl8 parseSubExpression

-- Prefix expressions
-- can have any arbitrary list of expression suffixes
parsePrefixExp :: AParser PrefixExp
parsePrefixExp = pPrefixExp (pMany pPFExprSuffix)

-- Prefix expressions
-- The suffixes define rules on the allowed suffixes
pPrefixExp :: AParser [PFExprSuffix] -> AParser PrefixExp
pPrefixExp suffixes = PFVar <$> pName <*> (reverse <$> suffixes) <<|>
                      ExprVar <$ pMTok LRound <*> parseExpression <* pMTok RRound <*> (reverse <$> suffixes)

-- Parse any expression suffix
pPFExprSuffix :: AParser PFExprSuffix
pPFExprSuffix = Call <$> parseArgs <<|>
                MetaCall <$ pMTok Colon <*> pName <*> parseArgs <<|>
                ExprIndex <$ pMTok LSquare <*> parseExpression <* pMTok RSquare <<|>
                DotIndex <$ pMTok Dot <*> pName

-- Function calls are prefix expressions, but the last suffix MUST be either a function call or a metafunction call
pFunctionCall :: AParser PrefixExp
pFunctionCall = pPrefixExp suffixes
    where
        suffixes = (:) <$> pPFExprSuffix <*> suffixes <|>
                   (flip (:) [] . Call) <$> parseArgs <|>
                   (\n a -> [MetaCall n a]) <$ pMTok Colon <*> pName <*> parseArgs


-- arguments of a function call (including brackets)
parseArgs :: AParser Args
parseArgs = ListArgs <$ pMTok LRound <*> opt parseExpressionList [] <* pMTok RRound <<|>
            TableArg <$> parseTableConstructor <<|>
            StringArg <$> parseString

-- Table constructor
parseTableConstructor :: AParser [Field]
parseTableConstructor = pMTok LCurly *> (parseFieldList <<|> pReturn []) <* pMTok RCurly

-- A list of table entries
-- Grammar: field {separator field} [separator]
parseFieldList :: AParser [Field]
parseFieldList = parseField <**> (
                    parseFieldSep <**> (
                        (\flist _ field -> field : flist) <$> parseFieldList <<|>
                        pReturn (\_ f -> [f])
                    ) <<|>
                    pReturn (: [])
                )

-- A field in a table
parseField :: AParser Field
parseField = ExprField <$ pMTok LSquare <*> parseExpression <* pMTok RSquare <* pMTok Equals <*> parseExpression <<|>
             (NamedField <$> pName <* pMTok Equals <*> parseExpression <|>
              UnnamedField <$> parseExpression)

-- Field separator
parseFieldSep :: AParser MToken
parseFieldSep = pMTok Comma <<|> pMTok Semicolon

-- Parse unary operator (-, not, #)
parseUnOp :: AParser UnOp
parseUnOp = UnMinus <$ pMTok Minus <<|>
            ANot    <$ pMTok Not   <<|>
            ANot    <$ pMTok CNot  <<|>
            AHash   <$ pMTok Hash

-- Parses the full AST
-- Its first parameter contains all comments
-- Assumes the mtokens fed to the AParser have no comments
parseChunk :: [MToken] -> AParser AST
parseChunk cms = AST cms <$> parseBlock

parseGLua :: [MToken] -> (AST, [Error LineColPos])
parseGLua mts = let (cms, ts) = splitComments mts in
                 execAParser (parseChunk cms) ts
