{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module GLua.Parser where

import GLua.AG.AST (
  AReturn (..),
  AST (..),
  Args (..),
  BinOp (..),
  Block (..),
  Expr (..),
  Field (..),
  FieldSep (..),
  FuncName (..),
  MElse (MElse),
  MElseIf (MElseIf),
  MExpr (..),
  MStat (..),
  PFExprSuffix (..),
  PrefixExp (..),
  Stat (..),
  UnOp (..),
 )
import GLua.AG.Token (
  MToken (..),
  Token (..),
 )
import qualified GLua.Lexer as Lex
import GLua.TokenTypes (
  isWhitespace,
  mpos,
  splitComments,
  tokenSize,
 )

import GLua.Position (LineColPos (..), Region (..))
import Text.Parsec (
  ParseError,
  Parsec,
  SourceName,
  SourcePos,
  anyToken,
  between,
  chainl1,
  choice,
  eof,
  getPosition,
  getState,
  incSourceColumn,
  lookAhead,
  many,
  many1,
  option,
  optionMaybe,
  putState,
  runParser,
  sepBy1,
  sourceColumn,
  sourceLine,
  tokenPrim,
  try,
  unexpected,
  (<?>),
  (<|>),
 )
import Text.Parsec.Pos (newPos)

type AParser = Parsec [MToken] LineColPos

-- | Execute a parser
execAParser :: SourceName -> AParser a -> [MToken] -> Either ParseError a
execAParser name p = runParser p (LineColPos 0 0 0) name

-- | Parse Garry's mod Lua tokens to an abstract syntax tree.
-- Also returns parse errors
parseGLua :: [MToken] -> Either ParseError AST
parseGLua mts =
  let
    (cms, ts) = splitComments . filter (not . isWhitespace) $ mts
  in
    execAParser "source.lua" (parseChunk cms) ts

parseGLuaFromString :: String -> Either ParseError AST
parseGLuaFromString contents =
  case Lex.execParseTokens contents of
    Left parseErrors -> Left parseErrors
    Right lexicon -> parseGLua $ filter (not . isWhitespace) lexicon

-- | Region start to SourcePos
rgStart2sp :: Region -> SourcePos
rgStart2sp (Region start _) = lcp2sp start

-- | Region end to SourcePos
rgEnd2sp :: Region -> SourcePos
rgEnd2sp (Region _ end) = lcp2sp end

-- | SourcePos to region
sp2Rg :: SourcePos -> Region
sp2Rg sp = Region (sp2lcp sp) (sp2lcp sp)

-- | LineColPos to SourcePos
lcp2sp :: LineColPos -> SourcePos
lcp2sp (LineColPos l c _) = newPos "source.lua" (l + 1) (c + 1)

-- | SourcePos to LineColPos
sp2lcp :: SourcePos -> LineColPos
sp2lcp pos = LineColPos (sourceLine pos - 1) (sourceColumn pos - 1) 0

-- | Update a SourcePos with an MToken
updatePosMToken :: SourcePos -> MToken -> [MToken] -> SourcePos
updatePosMToken _ (MToken p tok) [] = incSourceColumn (rgStart2sp p) (tokenSize tok)
updatePosMToken _ _ (MToken p _ : _) = rgStart2sp p

-- | Match a token
pMTok :: Token -> AParser MToken
pMTok tok = pMToken $ \mt@(MToken _ t) -> if t == tok then Just mt else Nothing

-- Tokens that satisfy a condition
pMSatisfy :: (MToken -> Bool) -> AParser MToken
pMSatisfy cond = pMToken $ \mt -> if cond mt then Just mt else Nothing

pMToken :: forall a. (MToken -> Maybe a) -> AParser a
pMToken cond = do
  (MToken pos _, res) <- tokenPrim (quote . show) updatePosMToken testMToken

  putState (rgEnd pos)

  pure res
  where
    testMToken :: MToken -> Maybe (MToken, a)
    testMToken mt = (mt,) <$> cond mt

    quote :: String -> String
    quote str = "\"" ++ str ++ "\""

-- | Get the source position.
-- Simply gets the start position of the next token.
-- Falls back on the collected position when there is no token left.
pPos :: AParser LineColPos
pPos = rgStart . mpos <$> lookAhead anyToken <|> sp2lcp <$> getPosition

-- | Get the source position
-- Simply gets the end position of the last parsed token
pEndPos :: AParser LineColPos
pEndPos = getState

-- | A thing of which the region is to be parsed
annotated :: (Region -> a -> b) -> AParser a -> AParser b
annotated f p = (\s t e -> f (Region s e) t) <$> pPos <*> p <*> pEndPos

-- | Parses the full AST
-- Its first parameter contains all comments
-- Assumes the mtokens fed to the AParser have no comments
parseChunk :: [MToken] -> AParser AST
parseChunk cms = AST cms <$> parseBlock <* eof

-- | Parse a block with an optional return value
parseBlock :: AParser Block
parseBlock = do
  -- using 'pEndPos' here, to make sure the region starts right at the end of the last token. This
  -- is to make sure it captures the whitespace too.
  start <- pEndPos
  mStats <- pInterleaved (pMTok Semicolon) parseMStat
  returnStatement <- parseReturn <|> return NoReturn
  -- Using 'pPos' here, to make sure the region also includes the whitespace
  end <- pPos
  pure $ Block (Region start end) mStats returnStatement

parseMStat :: AParser MStat
parseMStat = annotated MStat parseStat

-- | Parser that is interleaved with 0 or more of the other parser
pInterleaved :: AParser a -> AParser b -> AParser [b]
pInterleaved sep q = many sep *> many (q <* many sep)

-- | Parse a return value
parseReturn :: AParser AReturn
parseReturn = annotated AReturn (pMTok Return *> option [] parseExpressionList <* many (pMTok Semicolon) <?> "return statement")

-- | Label
parseLabel :: AParser MToken
parseLabel = pMSatisfy isLabel <?> "'label'"
  where
    isLabel :: MToken -> Bool
    isLabel (MToken _ (Label{})) = True
    isLabel _ = False

-- | Parse a single statement
parseStat :: AParser Stat
parseStat =
  ALabel <$> parseLabel
    <|> ABreak <$ pMTok Break
    <|> AContinue <$ pMTok Continue
    <|> ADo <$ pMTok Do <*> parseBlock <* pMTok End
    <|> AWhile <$ pMTok While <*> parseExpression <* pMTok Do <*> parseBlock <* pMTok End
    <|> ARepeat <$ pMTok Repeat <*> parseBlock <* pMTok Until <*> parseExpression
    <|> parseIf
    <|> parseFunction
    <|> parseFor
    <|> try (AGoto <$ pMTok (Identifier "goto") <*> pName)
    <|> parseDefinition
    <|> AFuncCall <$> pFunctionCall
    <|> pMTok Local
      *> ( parseLocalDefinition
            <|> parseLocalFunction
         )

-- | Global definition
-- Note: Uses try to avoid conflicts with function calls
parseDefinition :: AParser Stat
parseDefinition = flip (<?>) "variable definition" $ do
  vars <- try $ do
    vs <- parseVarList
    _ <- pMTok Equals
    return vs

  exprs <- parseExpressionList

  return $ Def (zip vars (map Just exprs ++ repeat Nothing))

-- | Local definition
parseLocalDefinition :: AParser Stat
parseLocalDefinition = def <$> parseLocalVarList <*> option [] (pMTok Equals *> parseExpressionList) <?> "variable declaration"
  where
    def :: [PrefixExp] -> [MExpr] -> Stat
    def ps exs = LocDef $ zip ps (map Just exs ++ repeat Nothing)

-- | Global function definition
parseFunction :: AParser Stat
parseFunction =
  AFunc
    <$ pMTok Function
    <*> parseFuncName
    <*> between (pMTok LRound) (pMTok RRound) parseParList
    <*> parseBlock
    <* pMTok End
    <?> "function definition"

-- | Local function definition
parseLocalFunction :: AParser Stat
parseLocalFunction =
  ALocFunc
    <$ pMTok Function
    <*> parseLocFuncName
    <*> between (pMTok LRound) (pMTok RRound) parseParList
    <*> parseBlock
    <* pMTok End
    <?> "local function definition"

-- | Parse if then elseif then else end expressions
parseIf :: AParser Stat
parseIf =
  AIf
    <$ pMTok If
    <*> parseExpression
    <* pMTok Then
    <*> parseBlock
    <*>
    -- elseif
    many (annotated MElseIf $ (,) <$ pMTok Elseif <*> parseExpression <* pMTok Then <*> parseBlock)
    <*>
    -- else
    optionMaybe (annotated MElse $ pMTok Else *> parseBlock)
    <* pMTok End
    <?> "if statement"

parseFor :: AParser Stat
parseFor = parseNFor <|> parseGFor

-- | Parse numeric for loop
parseNFor :: AParser Stat
parseNFor = flip (<?>) "numeric for loop" $
  do
    name <- try $ do
      _ <- pMTok For
      name <- pName
      _ <- pMTok Equals
      return name

    start <- parseExpression
    _ <- pMTok Comma
    to <- parseExpression
    st <- step
    _ <- pMTok Do
    blk <- parseBlock
    _ <- pMTok End

    return $ ANFor name start to st blk
  where
    step :: AParser MExpr
    step = pMTok Comma *> parseExpression <|> annotated MExpr (return (ANumber "1"))

-- | Generic for loop
parseGFor :: AParser Stat
parseGFor = AGFor <$ pMTok For <*> parseNameList <* pMTok In <*> parseExpressionList <* pMTok Do <*> parseBlock <* pMTok End <?> "generic for loop"

-- | Function name (includes dot indices and meta indices)
parseFuncName :: AParser FuncName
parseFuncName =
  (\a b c -> FuncName (a : b) c)
    <$> pName
    <*> many (pMTok Dot *> pName)
    <*> option Nothing (Just <$ pMTok Colon <*> pName)
    <?> "function name"

-- | Local function name: cannot be a meta function nor indexed
parseLocFuncName :: AParser FuncName
parseLocFuncName = (\name -> FuncName [name] Nothing) <$> pName <?> "function name"

-- | Parse a number into an expression
parseNumber :: AParser Expr
parseNumber = pMToken isNumber <?> "number"
  where
    isNumber :: MToken -> Maybe Expr
    isNumber = \case
      MToken _ (TNumber str) -> Just $ ANumber str
      _ -> Nothing

-- | Parse any kind of string
parseString :: AParser MToken
parseString = pMSatisfy isString <?> "string"
  where
    isString :: MToken -> Bool
    isString (MToken _ (DQString _)) = True
    isString (MToken _ (SQString _)) = True
    isString (MToken _ (MLString _)) = True
    isString _ = False

-- | Parse an identifier
pName :: AParser MToken
pName = pMSatisfy isName <?> "identifier"
  where
    isName :: MToken -> Bool
    isName (MToken _ (Identifier _)) = True
    isName _ = False

-- | Parse a list of identifiers
parseNameList :: AParser [MToken]
parseNameList = sepBy1 pName (pMTok Comma)

-- | Parse variable list (var1, var2, var3)
parseVarList :: AParser [PrefixExp]
parseVarList = sepBy1 parseVar (pMTok Comma)

-- | Parse local variable list (var1, var2, var3)
parseLocalVarList :: AParser [PrefixExp]
parseLocalVarList = sepBy1 (PFVar <$> pName <*> pure []) (pMTok Comma)

-- | Parse list of function parameters
parseParList :: AParser [MToken]
parseParList = option [] $ nameParam <|> vararg
  where
    vararg = (: []) <$> pMTok VarArg <?> "'...'"
    nameParam = (:) <$> pName <*> moreParams <?> "parameter"
    moreParams = option [] $ pMTok Comma *> (nameParam <|> vararg)

-- | list of expressions
parseExpressionList :: AParser [MExpr]
parseExpressionList = sepBy1 parseExpression (pMTok Comma)

-- | Subexpressions, i.e. without operators
parseSubExpression :: AParser Expr
parseSubExpression =
  ANil
    <$ pMTok Nil
    <|> AFalse
    <$ pMTok TFalse
    <|> ATrue
    <$ pMTok TTrue
    <|> parseNumber
    <|> AString
    <$> parseString
    <|> AVarArg
    <$ pMTok VarArg
    <|> parseAnonymFunc
    <|> APrefixExpr
    <$> parsePrefixExp
    <|> ATableConstructor
    <$> parseTableConstructor
    <?> "expression"

-- | Separate parser for anonymous function subexpression
parseAnonymFunc :: AParser Expr
parseAnonymFunc =
  AnonymousFunc
    <$ pMTok Function
    <* pMTok LRound
    <*> parseParList
    <* pMTok RRound
    <*> parseBlock
    <* pMTok End
    <?> "anonymous function"

-- | Parse operators of the same precedence in a chain
samePrioL :: [(Token, BinOp)] -> AParser MExpr -> AParser MExpr
samePrioL ops pr = chainl1 pr (choice (map f ops))
  where
    f :: (Token, BinOp) -> AParser (MExpr -> MExpr -> MExpr)
    f (t, at) = annotated (\p _ e1 e2 -> MExpr p (BinOpExpr at e1 e2)) (pMTok t)

samePrioR :: [(Token, BinOp)] -> AParser MExpr -> AParser MExpr
samePrioR ops pr = chainl1 pr (choice (map f ops))
  where
    f :: (Token, BinOp) -> AParser (MExpr -> MExpr -> MExpr)
    f (t, at) = annotated (\p _ e1 e2 -> MExpr p (BinOpExpr at e1 e2)) (pMTok t)

-- | Parse unary operator (-, not, #)
parseUnOp :: AParser UnOp
parseUnOp =
  UnMinus <$ pMTok Minus
    <|> ANot <$ pMTok Not
    <|> ANot <$ pMTok CNot
    <|> AHash <$ pMTok Hash

-- | Parses a binary operator
parseBinOp :: AParser BinOp
parseBinOp =
  AOr <$ pMTok Or
    <|> AOr <$ pMTok COr
    <|> AAnd <$ pMTok And
    <|> AAnd <$ pMTok CAnd
    <|> ALT <$ pMTok TLT
    <|> AGT <$ pMTok TGT
    <|> ALEQ <$ pMTok TLEQ
    <|> AGEQ <$ pMTok TGEQ
    <|> ANEq <$ pMTok TNEq
    <|> ANEq <$ pMTok TCNEq
    <|> AEq <$ pMTok TEq
    <|> AConcatenate <$ pMTok Concatenate
    <|> APlus <$ pMTok Plus
    <|> BinMinus <$ pMTok Minus
    <|> AMultiply <$ pMTok Multiply
    <|> ADivide <$ pMTok Divide
    <|> AModulus <$ pMTok Modulus
    <|> APower <$ pMTok Power

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
parseExpression =
  samePrioL
    lvl1
    ( samePrioL lvl2 $
        samePrioL lvl3 $
          samePrioR lvl4 $
            samePrioL lvl5 $
              samePrioL lvl6 $
                annotated MExpr (UnOpExpr <$> parseUnOp <*> parseExpression)
                  <|> samePrioR lvl8 (annotated MExpr (parseSubExpression <|> UnOpExpr <$> parseUnOp <*> parseExpression)) -- lvl7
    )
    <?> "expression"

-- | Prefix expressions
-- can have any arbitrary list of expression suffixes
parsePrefixExp :: AParser PrefixExp
parsePrefixExp = pPrefixExp (many pPFExprSuffix)

-- | Prefix expressions
-- The suffixes define rules on the allowed suffixes
pPrefixExp :: AParser [PFExprSuffix] -> AParser PrefixExp
pPrefixExp suffixes =
  PFVar <$> pName <*> suffixes
    <|> ExprVar <$ pMTok LRound <*> parseExpression <* pMTok RRound <*> suffixes

-- | Parse any expression suffix
pPFExprSuffix :: AParser PFExprSuffix
pPFExprSuffix = pPFExprCallSuffix <|> pPFExprIndexSuffix

-- | Parse an indexing expression suffix
pPFExprCallSuffix :: AParser PFExprSuffix
pPFExprCallSuffix =
  Call
    <$> parseArgs
    <|> MetaCall
    <$ pMTok Colon
    <*> pName
    <*> parseArgs
    <?> "function call"

-- | Parse an indexing expression suffix
pPFExprIndexSuffix :: AParser PFExprSuffix
pPFExprIndexSuffix =
  ExprIndex
    <$ pMTok LSquare
    <*> parseExpression
    <* pMTok RSquare
    <|> DotIndex
    <$ pMTok Dot
    <*> pName
    <?> "indexation"

-- | Function calls are prefix expressions, but the last suffix MUST be either a function call or a metafunction call
pFunctionCall :: AParser PrefixExp
pFunctionCall = pPrefixExp suffixes <?> "function call"
  where
    suffixes =
      concat
        <$> many1
          ( (\ix c -> ix ++ [c]) <$> many1 pPFExprIndexSuffix <*> pPFExprCallSuffix
              <|> (: []) <$> pPFExprCallSuffix
          )

-- | single variable. Note: definition differs from reference to circumvent the left recursion
-- var ::= Name [{PFExprSuffix}* indexation] | '(' exp ')' {PFExprSuffix}* indexation
-- where "{PFExprSuffix}* indexation" is any arbitrary sequence of prefix expression suffixes that end with an indexation
parseVar :: AParser PrefixExp
parseVar = do
  variable <- pPrefixExp suffixes <?> "variable"

  -- Special case: A definition `(foo) = bar` is not allowed, but `(foo)[index] = bar` _is_ allowed.
  -- If it's an ExprVar, it must have suffixes.
  case variable of
    ExprVar _someExpression [] -> unexpected "expression in parentheses"
    _ -> pure variable
  where
    suffixes =
      concat
        <$> many
          ( (\c ix -> c ++ [ix]) <$> many1 pPFExprCallSuffix <*> pPFExprIndexSuffix
              <|> (: []) <$> pPFExprIndexSuffix
          )

-- | Arguments of a function call (including brackets)
parseArgs :: AParser Args
parseArgs =
  ListArgs
    <$ pMTok LRound
    <*> option [] parseExpressionList
    <* pMTok RRound
    <|> TableArg
    <$> parseTableConstructor
    <|> StringArg
    <$> parseString
    <?> "function arguments"

-- | Table constructor
parseTableConstructor :: AParser [Field]
parseTableConstructor = pMTok LCurly *> parseFieldList <* pMTok RCurly <?> "table"

-- | A list of table entries
-- Grammar: field {separator field} [separator]
parseFieldList :: AParser [Field]
parseFieldList = option [] $ do
  field <- parseField
  sep <- parseOptionalFieldSep
  case sep of
    NoSep -> pure [field NoSep]
    _ -> (field sep :) <$> parseFieldList

-- | Parse a named field (e.g. {named = field})
-- Contains try to avoid conflict with unnamed fields
parseNamedField :: AParser (FieldSep -> Field)
parseNamedField = do
  name <- try $ do
    n <- pName
    _ <- pMTok Equals
    return n

  NamedField name <$> parseExpression

-- | A field in a table
parseField :: AParser (FieldSep -> Field)
parseField =
  ExprField
    <$ pMTok LSquare
    <*> parseExpression
    <* pMTok RSquare
    <* pMTok Equals
    <*> parseExpression
    <|> parseNamedField
    <|> UnnamedField
    <$> parseExpression
    <?> "field"

-- | Field separator, either comma or semicolon
parseFieldSep :: AParser FieldSep
parseFieldSep =
  CommaSep <$ pMTok Comma
    <|> SemicolonSep <$ pMTok Semicolon

-- | Optional field separator, returns NoSep when no separator is found
-- Used at the end of a field list
parseOptionalFieldSep :: AParser FieldSep
parseOptionalFieldSep = option NoSep parseFieldSep
