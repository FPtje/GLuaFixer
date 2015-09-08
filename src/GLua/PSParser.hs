{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses #-}

module GLua.PSParser where

import GLua.TokenTypes
import GLua.AG.Token
import GLua.AG.AST
import qualified GLua.Lexer as Lex

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.ParserCombinators.UU.BasicInstances(LineColPos(..)) -- for LineColPos

type AParser = Parsec [MToken] ()

-- | Execute a parser
execAParser :: SourceName -> AParser a -> [MToken] -> Either ParseError a
execAParser name p mts = parse p name mts

-- | Parse a string directly
parseFromString :: AParser a -> String -> Either ParseError a
parseFromString p = execAParser "source.lua" p . filter (not . isWhitespace) . fst . Lex.execParseTokens

-- | Parse Garry's mod Lua tokens to an abstract syntax tree.
-- Also returns parse errors
parseGLua :: [MToken] -> Either ParseError AST
parseGLua mts = let (cms, ts) = splitComments . filter (not . isWhitespace) $ mts in
                 execAParser "source.lua" (parseChunk cms) ts

-- | LineColPos to SourcePos
lcp2sp :: LineColPos -> SourcePos
lcp2sp (LineColPos l c _) = newPos "source.lua" l c

sp2lcp :: SourcePos -> LineColPos
sp2lcp pos = LineColPos (sourceLine pos) (sourceColumn pos) 0

-- | Update a SourcePos with an MToken
updatePosMToken :: SourcePos -> MToken -> [MToken] -> SourcePos
updatePosMToken _ (MToken p _) _ = lcp2sp p

-- | Match a token
pMTok :: Token -> AParser MToken
pMTok tok = tokenPrim show updatePosMToken testMToken
    where
        testMToken :: MToken -> Maybe MToken
        testMToken mt@(MToken _ t) = if t == tok then Just mt else Nothing

-- Tokens that satisfy a condition
pMSatisfy :: (MToken -> Bool) -> AParser MToken
pMSatisfy cond = tokenPrim show updatePosMToken testMToken
    where
        testMToken :: MToken -> Maybe MToken
        testMToken mt = if cond mt then Just mt else Nothing



-- | Parses the full AST
-- Its first parameter contains all comments
-- Assumes the mtokens fed to the AParser have no comments
parseChunk :: [MToken] -> AParser AST
parseChunk cms = AST cms <$> parseBlock

-- | Parse a block with an optional return value
parseBlock :: AParser Block
parseBlock = undefined

-- source position
pPos :: AParser LineColPos
pPos = sp2lcp <$> getPosition
