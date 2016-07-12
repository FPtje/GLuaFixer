{-# LANGUAGE FlexibleInstances,
             BangPatterns,
             TypeFamilies,
             MultiParamTypeClasses #-}

module GLua.PSParser where

import GLua.TokenTypes
import qualified GLua.AG.Token as T
import GLua.AG.AST
import qualified GLua.Lexer as Lex

import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.ParserCombinators.UU.BasicInstances(LineColPos(..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

type AParser = Parsec Dec [T.MToken]


instance Stream [T.MToken] where
  type Token [T.MToken] = T.MToken
  uncons [] = Nothing
  uncons (t:ts) = Just (t, ts)
  {-# INLINE uncons #-}
  updatePos _ _ _ (T.MToken rg _) = (lcp2sp $ rgStart rg, lcp2sp $ rgEnd rg)
  {-# INLINE updatePos #-}

-- | Execute a parser
execAParser :: String -> AParser a -> [T.MToken] -> Either (ParseError (Token [T.MToken]) Dec) a
execAParser name p = runParser p name

-- | Parse a string directly
parseFromString :: AParser a -> String -> Either (ParseError (Token [T.MToken]) Dec) a
parseFromString p = execAParser "source.lua" p . filter (not . isWhitespace) . fst . Lex.execParseTokens

-- | Parse Garry's mod Lua tokens to an abstract syntax tree.
-- Also returns parse errors
parseGLua :: [T.MToken] -> Either (ParseError (Token [T.MToken]) Dec) AST
parseGLua mts = let (cms, ts) = splitComments . filter (not . isWhitespace) $ mts in
                 execAParser "source.lua" (parseChunk cms) ts

-- | Region start to SourcePos
rgStart2sp :: T.Region -> SourcePos
rgStart2sp (T.Region start _) = lcp2sp start

-- | Region end to SourcePos
rgEnd2sp :: T.Region -> SourcePos
rgEnd2sp (T.Region _ end) = lcp2sp end

-- | SourcePos to region
sp2Rg :: SourcePos -> T.Region
sp2Rg sp = T.Region (sp2lcp sp) (sp2lcp sp)


-- | LineColPos to SourcePos
lcp2sp :: LineColPos -> SourcePos
lcp2sp (LineColPos l c _) = SourcePos "source.lua" (unsafePos $ fromIntegral l) (unsafePos $ fromIntegral c)

-- | SourcePos to LineColPos
sp2lcp :: SourcePos -> LineColPos
sp2lcp !pos = LineColPos (fromIntegral (unPos $ sourceLine pos)) (fromIntegral (unPos $ sourceColumn pos)) 0

-- | Satisfy primitive
pMSatisfy :: (T.MToken -> Bool) -> AParser T.MToken
pMSatisfy f = token testToken Nothing
  where
    testToken x =
      if f x
        then Right x
        else Left (Set.singleton (Tokens (x :| [])), Set.empty, Set.empty)


-- | Match a token
pMTok :: T.Token -> AParser T.MToken
pMTok tok = pMSatisfy (\(T.MToken _ t) -> t == tok)

-- | Parse any token
anyToken :: AParser T.MToken
anyToken = pMSatisfy (const True)

-- | Get the source position
-- Simply gets the position of the next token
-- Falls back on the collected position when there is no token left
pPos :: AParser LineColPos
pPos = rgStart . mpos <$> lookAhead anyToken <|> sp2lcp <$> getPosition

-- | Get the source position
-- Simply gets the end position of the last parsed token
pEndPos :: AParser LineColPos
pEndPos =  -- getState
  do
    s <- getParserState
    return $ sp2lcp $ NE.head $ statePos s -- TODO: is head correct?


-- | A thing of which the region is to be parsed
annotated :: (T.Region -> a -> b) -> AParser a -> AParser b
annotated f p = (\s t e -> f (T.Region s e) t) <$> pPos <*> p <*> pEndPos

-- | Parses the full AST
-- Its first parameter contains all comments
-- Assumes the mtokens fed to the AParser have no comments
parseChunk :: [T.MToken] -> AParser AST
parseChunk cms = AST cms <$> parseBlock <* eof

-- | Parse a block with an optional return value
parseBlock :: AParser Block
parseBlock = Block <$> pInterleaved (pMTok T.Semicolon) parseMStat <*> (parseReturn <|> return NoReturn)

parseMStat :: AParser MStat
parseMStat = annotated MStat parseStat

-- | Parser that is interleaved with 0 or more of the other parser
pInterleaved :: AParser a -> AParser b -> AParser [b]
pInterleaved sep q = many sep *> many (q <* many sep)

-- | Parse a return value
parseReturn :: AParser AReturn
parseReturn = annotated AReturn (pMTok T.Return *> option [] parseExpressionList <* many (pMTok T.Semicolon) <?> "return statement")

-- | Label
parseLabel :: AParser T.MToken
parseLabel = pMSatisfy isLabel <?> "label"
    where
        isLabel :: T.MToken -> Bool
        isLabel (T.MToken _ (T.Label _)) = True
        isLabel _ = False

-- | Parse a single statement
parseStat :: AParser Stat
parseStat = ALabel <$> parseLabel <|>
            ABreak <$ pMTok T.Break <|>
            AContinue <$ pMTok T.Continue <|>
            ADo <$ pMTok T.Do <*> parseBlock <* pMTok T.End <|>
            AWhile <$ pMTok T.While <*> parseExpression <* pMTok T.Do <*> parseBlock <* pMTok T.End <|>
            ARepeat <$ pMTok T.Repeat <*> parseBlock <* pMTok T.Until <*> parseExpression <|>
            parseIf <|>
            parseFunction <|>
            parseFor <|>
            try (AGoto <$ pMTok (T.Identifier "goto") <*> pName) <|>
            parseDefinition <|>
            AFuncCall <$> pFunctionCall <|>
            pMTok T.Local *>
                (parseLocalDefinition <|>
                parseLocalFunction)


-- | Global definition
-- Note: Uses try to avoid conflicts with function calls
parseDefinition :: AParser Stat
parseDefinition = flip (<?>) "variable definition" $ do
    vars <- try $ do
        vs <- parseVarList
        pMTok T.Equals
        return vs

    exprs <- parseExpressionList

    return $ Def (zip vars (map Just exprs ++ repeat Nothing))

-- | Local definition
parseLocalDefinition :: AParser Stat
parseLocalDefinition = def <$> parseLocalVarList <*> option [] (pMTok T.Equals *> parseExpressionList) <?> "variable declaration"
    where
        def :: [PrefixExp] -> [MExpr] -> Stat
        def ps exs = LocDef $ zip ps (map Just exs ++ repeat Nothing)

-- | Global function definition
parseFunction :: AParser Stat
parseFunction = AFunc <$ pMTok T.Function <*> parseFuncName <*>
                     between (pMTok T.LRound) (pMTok T.RRound) parseParList <*>
                     parseBlock <*
                     pMTok T.End <?> "function definition"

-- | Local function definition
parseLocalFunction :: AParser Stat
parseLocalFunction = ALocFunc <$ pMTok T.Function <*> parseLocFuncName <*>
                     between (pMTok T.LRound) (pMTok T.RRound) parseParList <*>
                     parseBlock <*
                     pMTok T.End <?> "local function definition"

-- | Parse if then elseif then else end expressions
parseIf :: AParser Stat
parseIf = AIf <$ pMTok T.If <*> parseExpression <* pMTok T.Then <*>
            parseBlock <*>
            -- elseif
            many ((,) <$ pMTok T.Elseif <*> parseExpression <* pMTok T.Then <*> parseBlock) <*>
            -- else
            optional (pMTok T.Else *> parseBlock) <*
            pMTok T.End <?> "if statement"

parseFor :: AParser Stat
parseFor = parseNFor <|> parseGFor

-- | Parse numeric for loop
parseNFor :: AParser Stat
parseNFor = flip (<?>) "numeric for loop" $
    do
        name <- try $ do
            pMTok T.For
            name <- pName
            pMTok T.Equals
            return name

        start <- parseExpression
        pMTok T.Comma
        to <- parseExpression
        st <- step
        pMTok T.Do
        blk <- parseBlock
        pMTok T.End

        return $ ANFor name start to st blk
    where
        step :: AParser MExpr
        step = pMTok T.Comma *> parseExpression <|> annotated MExpr (return (ANumber "1"))

-- | Generic for loop
parseGFor :: AParser Stat
parseGFor = AGFor <$ pMTok T.For <*> parseNameList <* pMTok T.In <*> parseExpressionList <* pMTok T.Do <*> parseBlock <* pMTok T.End <?> "generic for loop"

-- | T.Function name (includes dot indices and meta indices)
parseFuncName :: AParser FuncName
parseFuncName = (\a b c -> FuncName (a:b) c) <$> pName <*> many (pMTok T.Dot *> pName) <*>
                option Nothing (Just <$ pMTok T.Colon <*> pName) <?> "function name"

-- | Local function name: cannot be a meta function nor indexed
parseLocFuncName :: AParser FuncName
parseLocFuncName = (\name -> FuncName [name] Nothing) <$> pName <?> "function name"

-- | Parse a number into an expression
parseNumber :: AParser Expr
parseNumber = (\(T.MToken _ (T.TNumber str)) -> ANumber str) <$> pMSatisfy isNumber <?> "number"
    where
        isNumber :: T.MToken -> Bool
        isNumber (T.MToken _ (T.TNumber _)) = True
        isNumber _ = False

-- | Parse any kind of string
parseString :: AParser T.MToken
parseString = pMSatisfy isString <?> "string"
    where
        isString :: T.MToken -> Bool
        isString (T.MToken _ (T.DQString _)) = True
        isString (T.MToken _ (T.SQString _)) = True
        isString (T.MToken _ (T.MLString _)) = True
        isString _ = False

-- | Parse an identifier
pName :: AParser T.MToken
pName = pMSatisfy isName <?> "identifier"
    where
        isName :: T.MToken -> Bool
        isName (T.MToken _ (T.Identifier _)) = True
        isName _ = False

-- | Parse a list of identifiers
parseNameList :: AParser [T.MToken]
parseNameList = sepBy1 pName (pMTok T.Comma)

-- | Parse variable list (var1, var2, var3)
parseVarList :: AParser [PrefixExp]
parseVarList = sepBy1 parseVar (pMTok T.Comma)

-- | Parse local variable list (var1, var2, var3)
parseLocalVarList :: AParser [PrefixExp]
parseLocalVarList = sepBy1 (PFVar <$> pName <*> pure []) (pMTok T.Comma)

-- | Parse list of function parameters
parseParList :: AParser [T.MToken]
parseParList = option [] $ nameParam <|> vararg
    where
        vararg = (:[]) <$> pMTok T.VarArg <?> "..."
        nameParam = (:) <$> pName <*> moreParams <?> "parameter"
        moreParams = option [] $ pMTok T.Comma *> (nameParam <|> vararg)

-- | list of expressions
parseExpressionList :: AParser [MExpr]
parseExpressionList = sepBy1 parseExpression (pMTok T.Comma)

-- | Subexpressions, i.e. without operators
parseSubExpression :: AParser Expr
parseSubExpression = ANil <$ pMTok T.Nil <|>
                  AFalse <$ pMTok T.TFalse <|>
                  ATrue <$ pMTok T.TTrue <|>
                  parseNumber <|>
                  AString <$> parseString <|>
                  AVarArg <$ pMTok T.VarArg <|>
                  parseAnonymFunc <|>
                  APrefixExpr <$> parsePrefixExp <|>
                  ATableConstructor <$> parseTableConstructor <?> "expression"


-- | Separate parser for anonymous function subexpression
parseAnonymFunc :: AParser Expr
parseAnonymFunc = AnonymousFunc <$
                   pMTok T.Function <*
                   pMTok T.LRound <*> parseParList <* pMTok T.RRound <*>
                   parseBlock <*
                   pMTok T.End <?> "anonymous function"


parseExpression :: AParser MExpr
parseExpression =
    makeExprParser
        ((annotated MExpr parseSubExpression {-<|> UnOpExpr <$> parseUnOp <*> parseExpression-}) <?> "expression")
        [ [binary T.Power APower]
        , [prefix T.Minus UnMinus, prefix T.Not ANot, prefix T.CNot ANot, prefix T.Hash AHash]
        , [binary T.Multiply AMultiply, binary T.Divide ADivide, binary T.Modulus AModulus]
        , [binary T.Plus APlus, binary T.Minus BinMinus]
        , [binary T.Concatenate AConcatenate]
        , [binary T.TLT ALT, binary T.TGT AGT, binary T.TLEQ ALEQ, binary T.TGEQ AGEQ, binary T.TNEq ANEq, binary T.TCNEq ANEq, binary T.TEq AEq]
        , [binary T.And AAnd, binary T.CAnd AAnd]
        , [binary T.Or AOr, binary T.COr AOr]
        ]

-- Binary left associative operators
binary :: T.Token -> BinOp -> Operator AParser MExpr
binary name op =
    InfixL ((\l@(MExpr rg _) r@(MExpr rg' _) -> MExpr (T.Region (rgStart rg) (rgEnd rg')) (BinOpExpr op l r)) <$ pMTok name)

-- Prefix unary operator
prefix :: T.Token -> UnOp -> Operator AParser MExpr
prefix name op =
    Prefix ((\(T.MToken rg _) e@(MExpr rg' _) -> MExpr (T.Region (rgStart rg) (rgEnd rg')) (UnOpExpr op e)) <$> pMTok name)

---- | Parse operators of the same precedence in a chain
--samePrioL :: [(T.Token, BinOp)] -> AParser MExpr -> AParser MExpr
--samePrioL ops pr = _ pr (choice (map f ops))
--  where
--    f :: (T.Token, BinOp) -> AParser (MExpr -> MExpr -> MExpr)
--    f (t, at) = annotated (\p _ e1 e2 -> MExpr p (BinOpExpr at e1 e2)) (pMTok t)

--samePrioR :: [(T.Token, BinOp)] -> AParser MExpr -> AParser MExpr
--samePrioR ops pr = _ pr (choice (map f ops))
--  where
--    f :: (T.Token, BinOp) -> AParser (MExpr -> MExpr -> MExpr)
--    f (t, at) = annotated (\p _ e1 e2 -> MExpr p (BinOpExpr at e1 e2)) (pMTok t)

---- | Parse unary operator (-, not, #)
--parseUnOp :: AParser UnOp
--parseUnOp = UnMinus <$ pMTok T.Minus <|>
--            ANot    <$ pMTok T.Not   <|>
--            ANot    <$ pMTok T.CNot  <|>
--            AHash   <$ pMTok T.Hash

---- | Parses a binary operator
--parseBinOp :: AParser BinOp
--parseBinOp = const AOr          <$> pMTok T.Or           <|>
--             const AOr          <$> pMTok T.COr          <|>
--             const AAnd         <$> pMTok T.And          <|>
--             const AAnd         <$> pMTok T.CAnd         <|>
--             const ALT          <$> pMTok T.TLT          <|>
--             const AGT          <$> pMTok T.TGT          <|>
--             const ALEQ         <$> pMTok T.TLEQ         <|>
--             const AGEQ         <$> pMTok T.TGEQ         <|>
--             const ANEq         <$> pMTok T.TNEq         <|>
--             const ANEq         <$> pMTok T.TCNEq        <|>
--             const AEq          <$> pMTok T.TEq          <|>
--             const AConcatenate <$> pMTok T.Concatenate  <|>
--             const APlus        <$> pMTok T.Plus         <|>
--             const BinMinus     <$> pMTok T.Minus        <|>
--             const AMultiply    <$> pMTok T.Multiply     <|>
--             const ADivide      <$> pMTok T.Divide       <|>
--             const AModulus     <$> pMTok T.Modulus      <|>
--             const APower       <$> pMTok T.Power

---- | Operators, sorted by priority
---- Priority from: http://www.lua.org/manual/5.2/manual.html#3.4.7
--lvl1, lvl2, lvl3, lvl4, lvl5, lvl6, lvl8 :: [(T.Token, BinOp)]
--lvl1 = [(T.Or, AOr), (T.COr, AOr)]
--lvl2 = [(T.And, AAnd), (T.CAnd, AAnd)]
--lvl3 = [(T.TLT, ALT), (T.TGT, AGT), (T.TLEQ, ALEQ), (T.TGEQ, AGEQ), (T.TNEq, ANEq), (T.TCNEq, ANEq), (T.TEq, AEq)]
--lvl4 = [(T.Concatenate, AConcatenate)]
--lvl5 = [(T.Plus, APlus), (T.Minus, BinMinus)]
--lvl6 = [(T.Multiply, AMultiply), (T.Divide, ADivide), (T.Modulus, AModulus)]
---- lvl7 is unary operators
--lvl8 = [(T.Power, APower)]


---- | Parse chains of binary and unary operators
--parseExpression :: AParser MExpr
--parseExpression =  samePrioL lvl1
--                  (samePrioL lvl2 $
--                   samePrioL lvl3 $
--                   samePrioR lvl4 $
--                   samePrioL lvl5 $
--                   samePrioL lvl6 $
--                   annotated MExpr (UnOpExpr <$> parseUnOp <*> parseExpression) <|> -- lvl7
--                   samePrioR lvl8 (annotated MExpr (parseSubExpression <|> UnOpExpr <$> parseUnOp <*> parseExpression))) <?> "expression"

-- | Prefix expressions
-- can have any arbitrary list of expression suffixes
parsePrefixExp :: AParser PrefixExp
parsePrefixExp = pPrefixExp (many pPFExprSuffix)

-- | Prefix expressions
-- The suffixes define rules on the allowed suffixes
pPrefixExp :: AParser [PFExprSuffix] -> AParser PrefixExp
pPrefixExp suffixes = PFVar <$> pName <*> suffixes <|>
                      ExprVar <$ pMTok T.LRound <*> parseExpression <* pMTok T.RRound <*> suffixes

-- | Parse any expression suffix
pPFExprSuffix :: AParser PFExprSuffix
pPFExprSuffix = pPFExprCallSuffix <|> pPFExprIndexSuffix

-- | Parse an indexing expression suffix
pPFExprCallSuffix :: AParser PFExprSuffix
pPFExprCallSuffix = Call <$> parseArgs <|>
                    MetaCall <$ pMTok T.Colon <*> pName <*> parseArgs <?> "function call"

-- | Parse an indexing expression suffix
pPFExprIndexSuffix :: AParser PFExprSuffix
pPFExprIndexSuffix = ExprIndex <$ pMTok T.LSquare <*> parseExpression <* pMTok T.RSquare <|>
                     DotIndex <$ pMTok T.Dot <*> pName <?> "indexation"

-- | T.Function calls are prefix expressions, but the last suffix MUST be either a function call or a metafunction call
pFunctionCall :: AParser PrefixExp
pFunctionCall = pPrefixExp suffixes <?> "function call"
    where
        suffixes = concat <$> some ((\ix c -> ix ++ [c]) <$> some pPFExprIndexSuffix <*> pPFExprCallSuffix <|>
                                     (:[])                <$> pPFExprCallSuffix)

-- | single variable. Note: definition differs from reference to circumvent the left recursion
-- var ::= Name [{PFExprSuffix}* indexation] | '(' exp ')' {PFExprSuffix}* indexation
-- where "{PFExprSuffix}* indexation" is any arbitrary sequence of prefix expression suffixes that end with an indexation
parseVar :: AParser PrefixExp
parseVar = pPrefixExp suffixes <?> "variable"
    where
        suffixes = concat <$> many ((\c ix -> c ++ [ix]) <$> some pPFExprCallSuffix <*> pPFExprIndexSuffix <|>
                                    (:[])                <$> pPFExprIndexSuffix)

-- | Arguments of a function call (including brackets)
parseArgs :: AParser Args
parseArgs = ListArgs <$ pMTok T.LRound <*> option [] parseExpressionList <* pMTok T.RRound <|>
            TableArg <$> parseTableConstructor <|>
            StringArg <$> parseString <?> "function arguments"

-- | Table constructor
parseTableConstructor :: AParser [Field]
parseTableConstructor = pMTok T.LCurly *> parseFieldList <* pMTok T.RCurly <?> "table"

-- | A list of table entries
-- Grammar: field {separator field} [separator]
parseFieldList :: AParser [Field]
parseFieldList = sepEndBy parseField parseFieldSep <* many parseFieldSep

-- | Parse a named field (e.g. {named = field})
-- Contains try to avoid conflict with unnamed fields
parseNamedField :: AParser Field
parseNamedField = do
    name <- try $ do
        n <- pName
        pMTok T.Equals
        return n

    expr <- parseExpression

    return $ NamedField name expr

-- | A field in a table
parseField :: AParser Field
parseField = ExprField <$ pMTok T.LSquare <*> parseExpression <* pMTok T.RSquare <* pMTok T.Equals <*> parseExpression <|>
             parseNamedField <|>
             UnnamedField <$> parseExpression <?> "field"

-- | Field separator
parseFieldSep :: AParser T.MToken
parseFieldSep = pMTok T.Comma <|> pMTok T.Semicolon

