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

pMTok :: Token -> AParser MToken
pMTok t = pSatisfy isToken (Insertion ("Token " ++ show t) (MToken (TPos 0 0) Plus) 5)
    where
        isToken :: MToken -> Bool
        isToken (MToken _ tok) = t == tok

parseGLua :: [MToken] -> AST
parseGLua = const AST
