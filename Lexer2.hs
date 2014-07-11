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
        anyKeyword = pToken "true" <|> pToken "false" <|> pToken "nil" <|> pToken "not" <|> pToken "and" <|> pToken "or" <|> pToken "function" <|> pToken "local" <|> pToken "if" <|> pToken "then" <|> pToken "elseif" <|> pToken "else" <|> pToken "for" <|> pToken "in" <|> pToken "do" <|> pToken "while" <|> pToken "until" <|> pToken "repeat" <|> pToken "continue" <|> pToken "break" <|> pToken "return" <|> pToken "end" <|> pToken "goto"

parseLabel :: Parser String
parseLabel = pToken "::" *> parseIdentifier

lexToken :: Token -> String -> Parser [Token]
lexToken tok t = (tok :) . (: []) <$ pToken t <*> (Whitespace <$> parseWhitespace)

lexeme2 :: Parser Token -> Parser [Token]
lexeme2 p = (\a b -> [a, Whitespace b]) <$> p <*> parseWhitespace


parseToken :: Parser Token
parseToken =    Identifier <$> parseIdentifier <<|>
                (Whitespace <$> parseWhitespace      <|>
                parseComment                      <<|>

                -- Constants
                parseString                       <|>
                parseNumber                       <|>
                TTrue <$ pToken "true"                       <|>
                TFalse <$ pToken "false"                     <|>
                Nil <$ pToken "nil"                          <|>
                VarArg <$ pToken "..."                       <<|>

                -- Operators
                Concatenate <$ pToken ".."                   <<|>
                Dot <$ pToken "."                            <|>
                Plus <$ pToken "+"                           <|>
                Minus <$ pToken "-"                          <|>
                Mulitply <$ pToken "*"                       <|>
                Divide <$ pToken "/"                         <|>
                Modulus <$ pToken "%"                        <|>
                Power <$ pToken "^"                          <|>
                TEq <$ pToken "=="                           <<|>
                Equals <$ pToken "="                         <|>
                TNEq <$ pToken "~="                          <|>
                TCNEq <$ pToken "!="                         <<|>
                CNot <$ pToken "!"                           <|>
                TLEQ <$ pToken "<="                          <<|>
                TLT <$ pToken "<"                            <|>
                TGEQ <$ pToken ">="                          <<|>
                TGT <$ pToken ">"                            <|>
                Label <$> parseLabel              <<|>
                Colon <$ pToken ":"                          <|>
                Comma <$ pToken ","                          <|>
                Hash <$ pToken "#"                           <|>
                Not <$ pToken "not"                          <|>
                And <$ pToken "and"                          <|>
                CAnd <$ pToken "&&"                          <|>
                Or <$ pToken "or"                            <|>
                COr <$ pToken "||"                           <|>

                Function <$ pToken "function"                <|>
                Local <$ pToken "local"                      <|>
                If <$ pToken "if"                            <|>
                Then <$ pToken "then"                        <|>
                Elseif <$ pToken "elseif"                    <|>
                Else <$ pToken "else"                        <|>
                For <$ pToken "for"                          <|>
                In <$ pToken "in"                            <|>
                Do <$ pToken "do"                            <|>
                While <$ pToken "while"                      <|>
                Until <$ pToken "until"                      <|>
                Repeat <$ pToken "repeat"                    <|>
                Continue <$ pToken "continue"                <|>
                Break <$ pToken "break"                      <|>
                Return <$ pToken "return"                    <|>
                End <$ pToken "end"                          <|>
                Goto <$ pToken "goto"                        <|>

                LRound <$ pToken "("                      <|>
                RRound <$ pToken ")"                      <|>
                LCurly <$ pToken "{"                      <|>
                RCurly <$ pToken "}"                      <|>
                LSquare <$ pToken "["                     <|>
                RSquare <$ pToken "]")

parseTokens :: Parser [Token]
parseTokens = pMany parseToken


execParseTokens :: String -> ([Token], [Error LineColPos])
execParseTokens = execParser parseTokens
