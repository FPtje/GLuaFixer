{-# LANGUAGE FlexibleInstances,
            TypeSynonymInstances,
            MultiParamTypeClasses,
            Rank2Types, FlexibleContexts, NoMonomorphismRestriction,
            CPP #-}

-- | Parser based on <http://www.lua.org/manual/5.2/manual.html#9>
module GLua.Parser where

import GLua.TokenTypes
import GLua.AG.Token
import GLua.AG.AST
import qualified GLua.Lexer as Lex

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import qualified Data.ListLike as LL

-- | MTokens with the positions of the next MToken (used in the advance of parser)
data MTokenPos = MTokenPos MToken LineColPos

instance Show MTokenPos where
  show (MTokenPos tok _) = show tok

-- | Custom parser that parses MTokens
type AParser a = forall state. (IsLocationUpdatedBy LineColPos MTokenPos, LL.ListLike state MTokenPos) => P (Str MTokenPos state LineColPos) a

-- | LineColPos is a location that can be updated by MTokens
instance IsLocationUpdatedBy LineColPos MTokenPos where
    -- advance :: LineColPos -> MToken -> LineColPos
    -- Assume the position of the next MToken
    advance _ (MTokenPos _ p) = p


-- | Parse Garry's mod Lua tokens to an abstract syntax tree.
-- Also returns parse errors
parseGLua :: [MToken] -> (AST, [Error LineColPos])
parseGLua mts = let (cms, ts) = splitComments . filter (not . isWhitespace) $ mts in
                 execAParser (parseChunk cms) ts

-- | Parse a string directly into an AST
parseGLuaFromString :: String -> (AST, [Error LineColPos])
parseGLuaFromString = parseGLua . filter (not . isWhitespace) . fst . Lex.execParseTokens

-- | Parse a string directly
parseFromString :: AParser a -> String -> (a, [Error LineColPos])
parseFromString p = execAParser p . filter (not . isWhitespace) . fst . Lex.execParseTokens

-- | Create a parsable string from MTokens
createString :: [MToken] -> Str MTokenPos [MTokenPos] LineColPos
createString [] = createStr (LineColPos 0 0 0) []
createString mts@(MToken p _ : xs) = createStr p mtpos where
    mts' = xs ++ [last mts] -- Repeat last element of mts
    mkMtPos mt (MToken p' _) = MTokenPos mt p'
    mtpos = zipWith mkMtPos mts mts'

-- | Text.ParserCombinators.UU.Utils.execParser modified to parse MTokens
-- The first MToken might not be on the first line, so use the first MToken's position to start
execAParser :: AParser a -> [MToken] -> (a, [Error LineColPos])
execAParser p mts@[] = parse_h ((,) <$> p <*> pEnd) . createString $ mts
execAParser p mts@(_ : _) = parse_h ((,) <$> p <*> pEnd) . createString $ mts -- createStr (mpos m) $ mts


pMSatisfy :: (MToken -> Bool) -> Token -> String -> AParser MToken
pMSatisfy f t ins = getToken <$> pSatisfy f' (Insertion ins (MTokenPos (MToken ep t) ep) 5) where
    f' :: MTokenPos -> Bool
    f' (MTokenPos tok _) = f tok

    getToken :: MTokenPos -> MToken
    getToken (MTokenPos t' _) = t'

    ep = LineColPos 0 0 0

-- | Parse a single Metatoken, based on a positionless token (much like pSym)
pMTok :: Token -> AParser MToken
pMTok t = pMSatisfy isToken t $ "'" ++ show t ++ "'"
    where
        isToken :: MToken -> Bool
        isToken (MToken _ tok) = t == tok

-- | Parse a list of identifiers
parseNameList :: AParser [MToken]
parseNameList = (:) <$> pName <*> pMany (pMTok Comma *> pName)

-- | Parse list of function parameters
parseParList :: AParser [MToken]
parseParList = (pMTok VarArg <<|> pName) <**> (
                    pMTok Comma <**> (
                        (\a _ c -> [c, a]) <$> pMTok VarArg <<|>
                        (\a _ c -> c : a)  <$> parseParList
                    ) `opt` (: [])
               ) `opt` []

-- | Parses the full AST
-- Its first parameter contains all comments
-- Assumes the mtokens fed to the AParser have no comments
parseChunk :: [MToken] -> AParser AST
parseChunk cms = AST cms <$> parseBlock

-- | Parse a block with an optional return value
parseBlock :: AParser Block
parseBlock = Block <$> pInterleaved (pMTok Semicolon) parseMStat <*> (parseReturn <<|> pReturn NoReturn)

parseMStat :: AParser MStat
parseMStat = MStat <$> pPos <*> parseStat
-- | Parser that is interleaved with 0 or more of the other parser
pInterleaved :: AParser a -> AParser b -> AParser [b]
pInterleaved sep q = pMany sep *> pMany (q <* pMany sep)

-- | Behemoth parser that parses either function call statements or global declaration statements
-- Big in size and complexity because prefix expressions are BITCHES
-- The problem lies in the complexity of prefix expressions:
-- hotten.totten["tenten"](tentoonstelling) -- This is a function call
-- hotten.totten["tenten"] = tentoonstelling -- This is a declaration.
-- hotten.totten["tenten"], tentoonstelling = 1, 2 -- This is also a declaration.
-- One may find an arbitrary amount of expression suffixes (indexations/calls) before
-- finding a comma or equals sign that proves that it is a declaration.
parseCallDef :: AParser Stat
parseCallDef = (PFVar <$> pName <<|> -- Statemens begin with either a simple name or parenthesised expression
                ExprVar <$ pMTok LRound <*> parseExpression <* pMTok RRound) <**> (
                  -- Either there are more suffixes yet to be found (contSearch)
                  -- or there aren't and we will find either a comma or =-sign (varDecl namedVarDecl)
                  contSearch <<|>
                  varDecl namedVarDecl
                )
  where
    -- Simple direct declaration: varName, ... = 1, ...
    namedVarDecl :: [PrefixExp] -> LineColPos -> [MExpr] -> (ExprSuffixList -> PrefixExp) -> Stat
    namedVarDecl vars pos exprs pfe = let pfes = (pfe []) : vars in Def (zip pfes $ map Just exprs ++ repeat Nothing)

    -- This is where we know it's a variable declaration
    -- Takes a function that turns it into a proper Def Stat
    varDecl :: ([PrefixExp] -> LineColPos -> [MExpr] -> b) -> AParser b
    varDecl f = f <$> opt (pMTok Comma *> parseVarList) [] <*
              pMTok Equals <*>
              pPos <*>
              parseExpressionList

    -- We know that there is at least one suffix (indexation or call).
    -- Search for more suffixes and make either a call or declaration from it
    contSearch :: AParser ((ExprSuffixList -> PrefixExp) -> Stat)
    contSearch = (\(ss, mkStat) pfe -> mkStat $ pfe ss) <$> searchDeeper

    -- We either find a call suffix or an indexation suffix
    -- When it's a function call, try searching for more suffixes, if that doesn't work, it's a function call.
    -- When it's an indexation suffix, search for more suffixes or know that it's a declaration.
    searchDeeper :: AParser ([PFExprSuffix], PrefixExp -> Stat)
    searchDeeper = (pPFExprCallSuffix <**> (mergeDeeperSearch <$> searchDeeper <<|> pReturn (\s -> ([s], AFuncCall)))) <<|>
                   (pPFExprIndexSuffix <**> (mergeDeeperSearch <$> searchDeeper <<|> varDecl complexDecl))

    -- Merge the finding of more suffixes with the currently found suffix
    mergeDeeperSearch :: ([PFExprSuffix], PrefixExp -> Stat) -> PFExprSuffix -> ([PFExprSuffix], PrefixExp -> Stat)
    mergeDeeperSearch (ss, f) s = (s : ss, f)

    -- Multiple suffixes have been found, and proof has been found that this must be a declaration.
    -- Now to give all the collected suffixes and a function that creates the declaration
    complexDecl :: [PrefixExp] -> LineColPos -> [MExpr] -> PFExprSuffix -> ([PFExprSuffix], PrefixExp -> Stat)
    complexDecl vars pos exprs s = ([s], \pf -> Def (zip (pf : vars) $ map Just exprs ++ repeat Nothing))


-- | Parse a single statement
parseStat :: AParser Stat
parseStat = parseCallDef <<|>
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
              (\v (p,e) l -> LocDef (zip v $ map Just e ++ repeat Nothing)) <$> parseVarList <*> ((,) <$ pMTok Equals <*> pPos <*> parseExpressionList <<|> (,) <$> pPos <*> pReturn [])
            )


-- | Parse if then elseif then else end expressions
parseIf :: AParser Stat
parseIf = AIf <$ pMTok If <*> parseExpression <* pMTok Then <*>
            parseBlock <*>
            -- elseif
            pMany ((,) <$ pMTok Elseif <*> parseExpression <* pMTok Then <*> parseBlock) <*>
            -- else
            optional (pMTok Else *> parseBlock) <*
            pMTok End

-- | Parse numeric and generic for loops
parseFor :: AParser Stat
parseFor = do
  pMTok For
  firstName <- pName
  -- If you see an =-sign, it's a numeric for loop. It'll be a generic for loop otherwise
  isNumericLoop <- (const True <$> pMTok Equals <<|> const False <$> pReturn ())
  if isNumericLoop then do
      startExp <- parseExpression
      pMTok Comma
      toExp <- parseExpression
      step <- pMTok Comma *> parseExpression <<|> MExpr <$> pPos <*> pReturn (ANumber "1")
      pMTok Do
      block <- parseBlock
      pMTok End
      pReturn $ ANFor firstName startExp toExp step block
  else do
     vars <- (:) firstName <$ pMTok Comma <*> parseNameList <<|> pReturn [firstName]
     pMTok In
     exprs <- parseExpressionList
     pMTok Do
     block <- parseBlock
     pMTok End
     pReturn $ AGFor vars exprs block


-- | Parse a return value
parseReturn :: AParser AReturn
parseReturn = AReturn <$> pPos <* pMTok Return <*> opt parseExpressionList [] <* pMany (pMTok Semicolon)

-- | Label
parseLabel :: AParser MToken
parseLabel = pMSatisfy isLabel (Label "someLabel") "Some label"
    where
        isLabel :: MToken -> Bool
        isLabel (MToken _ (Label _)) = True
        isLabel _ = False

-- | Function name (includes dot indices and meta indices)
parseFuncName :: AParser FuncName
parseFuncName = (\a b c -> FuncName (a:b) c) <$> pName <*> pMany (pMTok Dot *> pName) <*>
                opt (Just <$ pMTok Colon <*> pName) Nothing

-- | Parse a number into an expression
parseNumber :: AParser Expr
parseNumber = (\(MToken _ (TNumber str)) -> ANumber str) <$> pMSatisfy isNumber (TNumber "0") "Number"
    where
        isNumber :: MToken -> Bool
        isNumber (MToken _ (TNumber str)) = True
        isNumber _ = False

-- | Parse any kind of string
parseString :: AParser MToken
parseString = pMSatisfy isString (DQString "someString") "String"
    where
        isString :: MToken -> Bool
        isString (MToken _ (DQString str)) = True
        isString (MToken _ (SQString str)) = True
        isString (MToken _ (MLString str)) = True
        isString _ = False

-- | Parse an identifier
pName :: AParser MToken
pName = pMSatisfy isName (Identifier "someVariable") "Variable"
    where
        isName :: MToken -> Bool
        isName (MToken _ (Identifier _)) = True
        isName _ = False

-- | Parse variable list (var1, var2, var3)
parseVarList :: AParser [PrefixExp]
parseVarList = pList1Sep (pMTok Comma) parseVar

-- | list of expressions
parseExpressionList :: AParser [MExpr]
parseExpressionList = pList1Sep (pMTok Comma) parseExpression

-- | Subexpressions, i.e. without operators
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

-- | Separate parser for anonymous function subexpression
parseAnonymFunc :: AParser Expr
parseAnonymFunc = AnonymousFunc <$
                   pMTok Function <*>
                   pPacked (pMTok LRound) (pMTok RRound) parseParList <*>
                   parseBlock <*
                   pMTok End

-- | Parse operators of the same precedence in a chain
samePrioL :: [(Token, BinOp)] -> AParser MExpr -> AParser MExpr
samePrioL ops pr = pChainl (choice (map f ops)) pr
  where
    choice = foldr (<<|>) pFail
    f :: (Token, BinOp) -> AParser (MExpr -> MExpr -> MExpr)
    f (t, at) = (\p e1 e2 -> MExpr p (BinOpExpr at e1 e2)) <$> pPos <* pMTok t

samePrioR :: [(Token, BinOp)] -> AParser MExpr -> AParser MExpr
samePrioR ops pr = pChainr (choice (map f ops)) pr
  where
    choice = foldr (<<|>) pFail
    f :: (Token, BinOp) -> AParser (MExpr -> MExpr -> MExpr)
    f (t, at) = (\p e1 e2 -> MExpr p (BinOpExpr at e1 e2)) <$> pPos <* pMTok t

-- | Parse unary operator (-, not, #)
parseUnOp :: AParser UnOp
parseUnOp = UnMinus <$ pMTok Minus <<|>
            ANot    <$ pMTok Not   <<|>
            ANot    <$ pMTok CNot  <<|>
            AHash   <$ pMTok Hash

-- | Operators, sorted by priority
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


-- | Parse chains of binary and unary operators
parseExpression :: AParser MExpr
parseExpression = samePrioL lvl1 $
                  samePrioL lvl2 $
                  samePrioL lvl3 $
                  samePrioR lvl4 $
                  samePrioL lvl5 $
                  samePrioL lvl6 $
                  MExpr <$> pPos <*> (UnOpExpr <$> parseUnOp <*> parseExpression) <<|> -- lvl7
                  samePrioR lvl8 (MExpr <$> pPos <*> parseSubExpression)

-- | Parses a binary operator
parseBinOp :: AParser BinOp
parseBinOp = const AOr          <$> pMTok Or           <<|>
             const AOr          <$> pMTok COr          <<|>
             const AAnd         <$> pMTok And          <<|>
             const AAnd         <$> pMTok CAnd         <<|>
             const ALT          <$> pMTok TLT          <<|>
             const AGT          <$> pMTok TGT          <<|>
             const ALEQ         <$> pMTok TLEQ         <<|>
             const AGEQ         <$> pMTok TGEQ         <<|>
             const ANEq         <$> pMTok TNEq         <<|>
             const ANEq         <$> pMTok TCNEq        <<|>
             const AEq          <$> pMTok TEq          <<|>
             const AConcatenate <$> pMTok Concatenate  <<|>
             const APlus        <$> pMTok Plus         <<|>
             const BinMinus     <$> pMTok Minus        <<|>
             const AMultiply    <$> pMTok Multiply     <<|>
             const ADivide      <$> pMTok Divide       <<|>
             const AModulus     <$> pMTok Modulus      <<|>
             const APower       <$> pMTok Power


-- | Prefix expressions
-- can have any arbitrary list of expression suffixes
parsePrefixExp :: AParser PrefixExp
parsePrefixExp = pPrefixExp (pMany pPFExprSuffix)

-- | Prefix expressions
-- The suffixes define rules on the allowed suffixes
pPrefixExp :: AParser [PFExprSuffix] -> AParser PrefixExp
pPrefixExp suffixes = PFVar <$> pName <*> suffixes <<|>
                      ExprVar <$ pMTok LRound <*> parseExpression <* pMTok RRound <*> suffixes

-- | Parse any expression suffix
pPFExprSuffix :: AParser PFExprSuffix
pPFExprSuffix = pPFExprCallSuffix <<|> pPFExprIndexSuffix

-- | Parse an indexing expression suffix
pPFExprCallSuffix :: AParser PFExprSuffix
pPFExprCallSuffix = Call <$> parseArgs <<|>
                    MetaCall <$ pMTok Colon <*> pName <*> parseArgs

-- | Parse an indexing expression suffix
pPFExprIndexSuffix :: AParser PFExprSuffix
pPFExprIndexSuffix = ExprIndex <$ pMTok LSquare <*> parseExpression <* pMTok RSquare <<|>
                     DotIndex <$ pMTok Dot <*> pName

-- | Function calls are prefix expressions, but the last suffix MUST be either a function call or a metafunction call
pFunctionCall :: AParser PrefixExp
pFunctionCall = pPrefixExp suffixes
    where
        suffixes = concat <$> pSome ((\ix c -> ix ++ [c]) <$> pSome pPFExprIndexSuffix <*> pPFExprCallSuffix <<|>
                                     (:[])                <$> pPFExprCallSuffix)

-- | single variable. Note: definition differs from reference to circumvent the left recursion
-- var ::= Name [{PFExprSuffix}* indexation] | '(' exp ')' {PFExprSuffix}* indexation
-- where "{PFExprSuffix}* indexation" is any arbitrary sequence of prefix expression suffixes that end with an indexation
parseVar :: AParser PrefixExp
parseVar = pPrefixExp suffixes
    where
        suffixes = concat <$> pMany ((\c ix -> c ++ [ix]) <$> pSome pPFExprCallSuffix <*> pPFExprIndexSuffix <<|>
                                     (:[])                <$> pPFExprIndexSuffix)

-- | Arguments of a function call (including brackets)
parseArgs :: AParser Args
parseArgs = ListArgs <$ pMTok LRound <*> opt parseExpressionList [] <* pMTok RRound <<|>
            TableArg <$> parseTableConstructor <<|>
            StringArg <$> parseString

-- | Table constructor
parseTableConstructor :: AParser [Field]
parseTableConstructor = pMTok LCurly *> parseFieldList <* pMTok RCurly

-- | A list of table entries
-- Grammar: field {separator field} [separator]
parseFieldList :: AParser [Field]
parseFieldList = (:) <$> parseField <*> otherFields <<|> pReturn []
  where
    otherFields :: AParser [Field]
    otherFields = parseFieldSep <**>
                    ((\f o _ -> f : o) <$> parseField <*> otherFields <<|> const <$> pReturn [])
                  <<|> pReturn []

-- | Makes an unnamed field out of a list of suffixes, a position and a name.
-- This function gets called when we know a field is unnamed and contains an expression that
-- starts with a PrefixExp
-- See the parseField parser where it is used
makeUnNamedField :: Maybe (BinOp, MExpr) -> ExprSuffixList -> (LineColPos, MToken) -> Field
makeUnNamedField Nothing sfs (p, nm) = UnnamedField $ MExpr p $ APrefixExpr $ PFVar nm sfs
makeUnNamedField (Just (op, mexpr)) sfs (p, nm) = UnnamedField $ MExpr p $ (merge (APrefixExpr $ PFVar nm sfs) mexpr)
  where
    -- Merge the first prefixExpr into the expression tree
    merge :: Expr -> MExpr -> Expr
    merge pf e@(MExpr _ (BinOpExpr op' l r)) =  if op > op' then
                                                  BinOpExpr op' (MExpr p $ (merge pf l)) r
                                                else
                                                  BinOpExpr op (MExpr p pf) e
    merge pf e = BinOpExpr op (MExpr p pf) e

-- | A field in a table
parseField :: AParser Field
parseField = ExprField <$ pMTok LSquare <*> parseExpression <* pMTok RSquare <* pMTok Equals <*> parseExpression <<|>
              ((,) <$> pPos <*> pName <**>
                -- Named field has equals sign immediately after the name
                (((\e (_, n) -> NamedField n e) <$ pMTok Equals <*> parseExpression) <<|>

                -- The lack of equals sign means it's an unnamed field.
                -- The expression of the unnamed field must be starting with a PFVar Prefix expression
                pMany pPFExprSuffix <**>
                  ( makeUnNamedField <$>
                    (
                      -- There are operators, so the expression goes on beyond the prefixExpression
                      (\op expr -> Just (op, expr)) <$> parseBinOp <*> parseExpression <<|>
                      -- There are no operators after the prefix expression
                      pReturn Nothing
                    )
                  )
                )
              ) <<|>
              UnnamedField <$> parseExpression

-- | Field separator
parseFieldSep :: AParser MToken
parseFieldSep = pMTok Comma <<|> pMTok Semicolon
