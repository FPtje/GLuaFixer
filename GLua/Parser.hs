{-# LANGUAGE FlexibleInstances,
            TypeSynonymInstances,
            MultiParamTypeClasses,
            Rank2Types, FlexibleContexts, NoMonomorphismRestriction,
            CPP #-}

module GLua.Parser where

import GLua.TokenTypes
import GLua.AST

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived
import qualified Data.ListLike as LL

-- Custom parser that parses MTokens
type AParser a = (IsLocationUpdatedBy TokenPos MToken, LL.ListLike state MToken) => P (Str MToken state TokenPos) a

-- TokenPos is a location that can be updated by MTokens
instance IsLocationUpdatedBy TokenPos MToken where
    -- advance :: TokenPos -> MToken3 -> TokenPos
    -- Recalculate pos in case a token was inserted by the error correction
    advance pos (MToken _ t) = addToken t pos

-- Text.ParserCombinators.UU.Utils.execParser modified to parse MTokens
execAParser :: AParser a -> [MToken] -> (a, [Error TokenPos])
execAParser p = parse_h ((,) <$> p <*> pEnd) . createStr (TPos 0 0)

-- Parse a single Metatoken, based on a positionless token (much like pSym)
pMTok :: Token -> AParser MToken
pMTok t = pSatisfy isToken (Insertion ("Token " ++ show t) (MToken (TPos 0 0) t) 5)
    where
        isToken :: MToken -> Bool
        isToken (MToken _ tok) = t == tok

-- Parse an identifier
pName :: AParser MToken
pName = pSatisfy isName (Insertion "Identifier" (MToken (TPos 0 0) (Identifier "something")) 5)
    where
        isName :: MToken -> Bool
        isName (MToken _ (Identifier _)) = True
        isName _ = False

-- Parse a namelist
parseNameList :: AParser [MToken]
parseNameList = (:) <$> pName <*> pMany (pMTok Comma *> pName)

-- Parse list of parameters
parseParList :: AParser [MToken]
{-parseParList = flip (:) [] <$> pMTok VarArg <<|>
               parseNameList <**>
                    (flip LL.snoc <$ pMTok Comma <*> pMTok VarArg <<|>
                    flip const <$> pReturn (MToken (TPos 0 0) Break))-}
parseParList = pName <**> (
                    pMTok Comma <**> (
                        (\a _ c -> [c, a]) <$> pMTok VarArg <<|>
                        (\a _ c -> c : a)  <$> parseParList
                    ) <<|>
                    pReturn (: [])
               )

-- Label
parseLabel :: AParser MToken
parseLabel = pSatisfy isLabel (Insertion "Label" (MToken (TPos 0 0) (Label "something")) 5)
    where
        isLabel :: MToken -> Bool
        isLabel (MToken _ (Label _)) = True
        isLabel _ = False

-- function name
parseFuncName :: AParser FuncName
parseFuncName = (\a b c -> FuncName (a:b) c) <$> pName <*> pMany (pMTok Dot *> pName) <*>
                opt (Just <$ pMTok Colon <*> pName) Nothing

parseNumber :: AParser Expr
parseNumber = (\(MToken _ (TNumber str)) -> ANumber str) <$> pSatisfy isNumber (Insertion "Number" (MToken (TPos 0 0) (TNumber "0")) 5)
    where
        isNumber :: MToken -> Bool
        isNumber (MToken _ (TNumber str)) = True
        isNumber _ = False

parseString :: AParser Expr
parseString = AString <$> pSatisfy isString (Insertion "String" (MToken (TPos 0 0) (DQString "0")) 5)
    where
        isString :: MToken -> Bool
        isString (MToken _ (DQString str)) = True
        isString (MToken _ (SQString str)) = True
        isString (MToken _ (MLString str)) = True
        isString _ = False


-- Expressions
parseExpression :: AParser Expr
parseExpression = ANil <$ pMTok Nil <<|>
                  AFalse <$ pMTok TFalse <<|>
                  ATrue <$ pMTok TTrue <<|>
                  parseNumber <<|>
                  parseString <<|>
                  AVarArg <$ pMTok VarArg
                  -- todo: AFunctionDef
                  -- todo: APrefixExpr
                  -- todo: ATableConstructor
                  -- todo: BinOp
                  -- todo: UnOp

-- Table constructor
parseTableConstructor :: AParser [Field]
parseTableConstructor = pMTok LCurly *> parseFieldList <* pMTok RCurly

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
             NamedField <$> pName <* pMTok Equals <*> parseExpression <<|>
             UnnamedField <$> parseExpression

-- Field separator
parseFieldSep :: AParser MToken
parseFieldSep = pMTok Comma <<|> pMTok Semicolon

-- Parse binary operator
parseBinOp :: AParser BinOp
parseBinOp =
    APlus          <$ pMTok Plus           <<|>
    BinMinus       <$ pMTok Minus          <<|>
    AMultiply      <$ pMTok Multiply       <<|>
    ADivide        <$ pMTok Divide         <<|>
    AModulus       <$ pMTok Modulus        <<|>
    APower         <$ pMTok Power          <<|>
    AConcatenate   <$ pMTok Concatenate    <<|>
    ALT            <$ pMTok TLT            <<|>
    ALEQ           <$ pMTok TLEQ           <<|>
    AGT            <$ pMTok TGT            <<|>
    AGEQ           <$ pMTok TGEQ           <<|>
    AEq            <$ pMTok TEq            <<|>
    ANEq           <$ pMTok TNEq           <<|>
    ANEq           <$ pMTok TCNEq          <<|>
    AAnd           <$ pMTok And            <<|>
    AAnd           <$ pMTok CAnd           <<|>
    AOr            <$ pMTok Or             <<|>
    AOr            <$ pMTok COr

-- Parse unary operator (-, not, #)
parseUnOp :: AParser UnOp
parseUnOp = UnMinus <$ pMTok Minus <<|>
            ANot    <$ pMTok Not   <<|>
            ANot    <$ pMTok CNot  <<|>
            AHash   <$ pMTok Hash

parseGLua :: [MToken] -> AST
parseGLua = const AST
