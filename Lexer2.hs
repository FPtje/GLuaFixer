{-# LANGUAGE FlexibleInstances,
            TypeSynonymInstances,
            MultiParamTypeClasses,
            Rank2Types, FlexibleContexts, NoMonomorphismRestriction,
            CPP #-}

module Lexer2 where

import TokenTypes

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived

parseWhitespace :: Parser String
parseWhitespace = pSome (pSym ' ' <|> pSym '\n' <|> pSym '\t' <|> pSym '\r')

pUntilEnd :: Parser String
pUntilEnd = pMunch (\c -> c /= '\n' && c /= '\r')

parseLineComment :: String -> Parser String
parseLineComment prefix = flip const <$> pToken prefix <*> pUntilEnd

parseAnyChar :: Parser Char
parseAnyChar = pSatisfy (const True) (Insertion "Any character" 'y' 5)

parseCBlockComment :: Parser String
parseCBlockComment = const "" <$> pToken "*/" <<|>
            (:) <$> parseAnyChar <*> parseCBlockComment

{-parseNestedString :: Parser String
parseNestedString = (\str -> "=" ++ str ++ "=") <$ pToken "=" <*> parseNestedString <* pToken "=" <|>
                    (\str -> "[" ++ str) <$ pToken "[" <*> endStr
                where endStr = pToken "]" <|> (:) <$> parseAnyChar <*> endStr-}

-- Parse a multiline string
parseMLString :: Parser String
parseMLString = (++) <$> pToken "[" <*> nested 0
    where
        -- The amount of =-signs in the string delimiter is n
        nested :: Int -> Parser String
        nested n = (\str -> "=" ++ str) <$ pToken "=" <*> nested (n + 1) <|>
                   ('[' :) <$ pToken "[" <*> restString n

        -- Right-recursive grammar. This part searches for the rest of the string until it finds the ]=^n] token
        restString :: Int -> Parser String
        restString n = pToken ("]" ++ replicate n '=' ++ "]") <<|>
                       (:) <$> parseAnyChar <*> restString n

parseDashBlockComment :: Parser String
parseDashBlockComment = pToken "--" *> parseMLString

parseComment :: Parser Token
parseComment =  DashBlockComment <$> parseDashBlockComment <<|>
                DashComment <$> parseLineComment "--" <|>
                SlashBlockComment <$ pToken "/*" <*> parseCBlockComment <|>
                SlashComment <$> parseLineComment "//"

-- Parse single line strings e.g. "sdf", 'werf'
parseLineString :: Char -> Parser String
parseLineString c = pSym c *> innerString
    where
        innerString :: Parser String
        innerString = (++) <$> pToken ['\\', c] <*> innerString <<|>
                      const "" <$> pSym c <<|>
                      (:) <$> parseAnyChar <*> innerString

parseString :: Parser Token
parseString = DQString <$> parseLineString '"' <|>
              SQString <$> parseLineString '\'' <|>
              MLString <$> parseMLString

parseNumber :: Parser Token
parseNumber = TNumber <$> ((++) <$> (pHexadecimal <|> pNumber) <*> opt pSuffix "")

    where
        pNumber :: Parser String
        pNumber = (++) <$> pSome pDigit <*> opt ((:) <$> pSym '.' <*> pSome pDigit) ""

        pHexadecimal :: Parser String
        pHexadecimal = (++) <$> pToken "0x" <*> pSome pHex

        pHex :: Parser Char
        pHex = pDigit <|> pSym 'a' <|> pSym 'b' <|> pSym 'c' <|> pSym 'd' <|> pSym 'e' <|> pSym 'f'
                      <|> pSym 'A' <|> pSym 'B' <|> pSym 'C' <|> pSym 'D' <|> pSym 'E' <|> pSym 'F'

        pSuffix :: Parser String
        pSuffix = (\e s d -> e : s ++ d) <$> (pSym 'e' <|> pSym 'E' <|> pSym 'p' <|> pSym 'P')
                    <*> opt (pToken "+" <|> pToken "-") ""
                    <*> pSome pDigit

parseIdentifier :: Parser String
-- parseIdentifier = (:) <$> (pSym '_' <|> pLetter) <*> pMany (pSym '_' <|> pLetter <|> pDigit)
parseIdentifier = (++) <$> anyKeyword <*> pSome allowed <<|>
                  (:)  <$> (pSym '_' <|> pLetter) <*> pMany allowed
    where
        allowed = pSym '_' <|> pLetter <|> pDigit
        anyKeyword :: Parser String
        anyKeyword = pToken "true" <|> pToken "false" <|> pToken "nil" <|> pToken "not" <|> pToken "and" <|> pToken "or" <|> pToken "function" <|> pToken "local" <|> pToken "if" <|> pToken "then" <|> pToken "elseif" <|> pToken "else" <|> pToken "for" <|> pToken "in" <|> pToken "do" <|> pToken "while" <|> pToken "until" <|> pToken "repeat" <|> pToken "break" <|> pToken "return" <|> pToken "end" <|> pToken "goto"

parseLabel :: Parser String
parseLabel = pToken "::" *> parseIdentifier

lexToken :: Token -> String -> Parser [Token]
lexToken tok t = (tok :) . (: []) <$ pToken t <*> (Whitespace <$> parseWhitespace)

lexeme2 :: Parser Token -> Parser [Token]
lexeme2 p = (\a b -> [a, Whitespace b]) <$> p <*> parseWhitespace


parseToken :: Parser [Token]
parseToken =    (:[]) . Whitespace <$> parseWhitespace      <|>
                (:[]) <$> parseComment                      <<|>

                -- Constants
                (:[]) <$> parseString                       <|>
                (:[]) <$> parseNumber                       <|>
                lexToken TTrue "true"                       <|>
                lexToken TFalse "false"                     <|>
                lexToken Nil "nil"                          <|>
                lexToken VarArg "..."                       <<|>

                -- Operators
                lexToken Concatenate ".."                   <<|>
                lexToken Dot "."                            <|>
                lexToken Plus "+"                           <|>
                lexToken Minus "-"                          <|>
                lexToken Mulitply "*"                       <|>
                lexToken Divide "/"                         <|>
                lexToken Modulus "%"                        <|>
                lexToken Power "^"                          <|>
                lexToken TEq "=="                           <<|>
                lexToken Equals "="                         <|>
                lexToken TNEq "~="                          <|>
                lexToken TCNEq "!="                         <<|>
                lexToken CNot "!"                           <|>
                lexToken TLEQ "<="                          <<|>
                lexToken TLT "<"                            <|>
                lexToken TGEQ ">="                          <<|>
                lexToken TGT ">"                            <|>
                lexeme2 (Label <$> parseLabel)              <<|>
                lexToken Colon ":"                          <|>
                lexToken Comma ","                          <|>
                lexToken Hash "#"                           <|>
                lexToken Not "not"                          <|>
                lexToken And "and"                          <|>
                lexToken CAnd "&&"                          <|>
                lexToken Or "or"                            <|>
                lexToken COr "||"                           <|>

                lexToken Function "function"                <|>
                lexToken Local "local"                      <|>
                lexToken If "if"                            <|>
                lexToken Then "then"                        <|>
                lexToken Elseif "elseif"                    <|>
                lexToken Else "else"                        <|>
                lexToken For "for"                          <|>
                lexToken In "in"                            <|>
                lexToken Do "do"                            <|>
                lexToken While "while"                      <|>
                lexToken Until "until"                      <|>
                lexToken Repeat "repeat"                    <|>
                lexToken Break "break"                      <|>
                lexToken Return "return"                    <|>
                lexToken End "end"                          <|>
                lexToken Goto "goto"                        <|>

                [LRound] <$ pToken "("                      <|>
                [RRound] <$ pToken ")"                      <|>
                [LCurly] <$ pToken "{"                      <|>
                [RCurly] <$ pToken "}"                      <|>
                [LSquare] <$ pToken "["                     <|>
                [RSquare] <$ pToken "]"                     <|>
                lexeme2 (Identifier <$> parseIdentifier)
                -- Number

parseTokens :: Parser [Token]
parseTokens = concat <$> pMany parseToken


execParseTokens :: String -> ([Token], [Error LineColPos])
execParseTokens = execParser parseTokens . (++ " ")
