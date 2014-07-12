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
parseWhitespace = pSome (pSym ' ' <<|> pSym '\n' <<|> pSym '\t' <<|> pSym '\r')

parseAnyChar :: Parser Char
parseAnyChar = pSatisfy (const True) (Insertion "Any character" 'y' 5)

parseCBlockComment :: Parser String
parseCBlockComment = const "" <$> pToken "*/" <<|>
            (:) <$> parseAnyChar <*> parseCBlockComment

-- Parse a multiline string
parseMLString :: Parser String
parseMLString = (++) <$> pToken "[" <*> nestedString

pUntilEnd :: Parser String
pUntilEnd = pMunch (\c -> c /= '\n' && c /= '\r')

parseLineComment :: String -> Parser String
parseLineComment prefix = flip const <$> pToken prefix <*> pUntilEnd

-- Parses a multiline string except for its first character (e.g. =[ string ]=])
-- This is because the first [ could also just be parsed as a square bracket.
nestedString :: Parser String
nestedString = nested 0
    where
        -- The amount of =-signs in the string delimiter is n
        nested :: Int -> Parser String
        nested n = (\str -> "=" ++ str) <$ pToken "=" <*> nested (n + 1) <<|>
                   ('[' :) <$ pToken "[" <*> restString n

        -- Right-recursive grammar. This part searches for the rest of the string until it finds the ]=^n] token
        restString :: Int -> Parser String
        restString n = pToken ("]" ++ replicate n '=' ++ "]") <<|>
                       (:) <$> parseAnyChar <*> restString n

parseComment :: Parser Token
parseComment =  pToken "--" <**> -- Dash block comment and dash comment both start with "--"
                    (const <$> (DashBlockComment <$> parseMLString <<|> DashComment <$> pUntilEnd)) <<|>
                pToken "/" <**>
                    (const <$>
                        (SlashBlockComment <$ pToken "*" <*> parseCBlockComment <<|>
                        SlashComment <$> parseLineComment "/" <<|>
                        pReturn Divide -- The /-sign is here because it also starts with '/'
                        ))

-- Parse single line strings e.g. "sdf", 'werf'
parseLineString :: Char -> Parser String
parseLineString c = pSym c *> innerString
    where
        innerString :: Parser String
        innerString = (\bs c str -> bs : c : str) <$> pSym '\\' <*> parseAnyChar <*> innerString <<|> -- escaped char
                      const "" <$> pSym c <<|>
                      (:) <$> parseAnyChar <*> innerString

parseString :: Parser Token
parseString = DQString <$> parseLineString '"' <<|>
              SQString <$> parseLineString '\'' <<|>
              -- Parse either a multiline string or just a bracket.
              -- Placed here because they have the first token '[' in common
              pSym '[' <**> ((\_ -> MLString . (:) '[') <$$> nestedString <<|>
                        const <$> pReturn LSquare)

parseNumber :: Parser Token
parseNumber = TNumber <$> ((++) <$> (pHexadecimal <<|> pNumber) <*> opt pSuffix "")

    where
        pNumber :: Parser String
        pNumber = (++) <$> pSome pDigit <*> opt ((:) <$> pSym '.' <*> pSome pDigit) ""

        pHexadecimal :: Parser String
        pHexadecimal = (++) <$> pToken "0x" <*> pSome pHex

        pHex :: Parser Char
        pHex = pDigit <<|> pSym 'a' <<|> pSym 'b' <<|> pSym 'c' <<|> pSym 'd' <<|> pSym 'e' <<|> pSym 'f'
                      <<|> pSym 'A' <<|> pSym 'B' <<|> pSym 'C' <<|> pSym 'D' <<|> pSym 'E' <<|> pSym 'F'

        pSuffix :: Parser String
        pSuffix = (\e s d -> e : s ++ d) <$> (pSym 'e' <<|> pSym 'E' <<|> pSym 'p' <<|> pSym 'P')
                    <*> opt (pToken "+" <<|> pToken "-") ""
                    <*> pSome pDigit

-- Parse a keyword. Note: It must really a key/word/! This parser makes sure to return an identifier when
-- it's actually an identifier that starts with that keyword
parseKeyword :: Token -> String -> Parser Token
parseKeyword tok word = pToken word <**>
                            ((\k -> Identifier . (++) k) <$$> pSome allowed <<|> const <$> pReturn tok)
    where
        allowed = pSym '_' <<|> pLetter <<|> pDigit

parseIdentifier :: Parser String
parseIdentifier = (:)  <$> (pSym '_' <<|> pLetter) <*> pMany allowed
    where
        allowed = pSym '_' <<|> pLetter <<|> pDigit

parseLabel :: Parser String
parseLabel = pToken "::" *> parseIdentifier

lexToken :: Token -> String -> Parser [Token]
lexToken tok t = (tok :) . (: []) <$ pToken t <*> (Whitespace <$> parseWhitespace)

lexeme2 :: Parser Token -> Parser [Token]
lexeme2 p = (\a b -> [a, Whitespace b]) <$> p <*> parseWhitespace

parseDots :: Parser Token
parseDots = pToken "." <**> ( -- A dot means it's either a VarArg (...), concatenation (..) or just a dot (.)
                const <$> (pToken "." <**> (
                    const VarArg <$ pToken "." <<|>
                    const <$> pReturn Concatenate
                )) <<|>
                const <$> pReturn Dot
                )

parseToken :: Parser Token
parseToken =    Whitespace <$> parseWhitespace               <<|>
                parseComment                                 <<|>

                -- Constants
                parseString                                  <<|>
                parseNumber                                  <<|>
                parseKeyword TTrue "true"                    <<|>
                parseKeyword TFalse "false"                  <<|>
                parseKeyword Nil "nil"                       <<|>
                parseKeyword Not "not"                       <<|>
                parseKeyword And "and"                       <<|>
                parseKeyword Or "or"                         <<|>
                parseKeyword Function "function"             <<|>
                parseKeyword Local "local"                   <<|>
                parseKeyword If "if"                         <<|>
                parseKeyword Then "then"                     <<|>
                parseKeyword Elseif "elseif"                 <<|>
                parseKeyword Else "else"                     <<|>
                parseKeyword For "for"                       <<|>
                parseKeyword In "in"                         <<|>
                parseKeyword Do "do"                         <<|>
                parseKeyword While "while"                   <<|>
                parseKeyword Until "until"                   <<|>
                parseKeyword Repeat "repeat"                 <<|>
                parseKeyword Continue "continue"             <<|>
                parseKeyword Break "break"                   <<|>
                parseKeyword Return "return"                 <<|>
                parseKeyword End "end"                       <<|>
                parseKeyword Goto "goto"                     <<|>

                Identifier <$> parseIdentifier               <<|>

                Semicolon <$ pToken ";"                      <<|>
                parseDots                                    <<|>

                -- Operators
                Plus <$ pToken "+"                           <<|>
                Minus <$ pToken "-"                          <<|>
                Mulitply <$ pToken "*"                       <<|>
                Modulus <$ pToken "%"                        <<|>
                Power <$ pToken "^"                          <<|>
                TEq <$ pToken "=="                           <<|>
                Equals <$ pToken "="                         <<|>
                TNEq <$ pToken "~="                          <<|>
                TCNEq <$ pToken "!="                         <<|>
                CNot <$ pToken "!"                           <<|>
                TLEQ <$ pToken "<="                          <<|>
                TLT <$ pToken "<"                            <<|>
                TGEQ <$ pToken ">="                          <<|>
                TGT <$ pToken ">"                            <<|>
                Label <$> parseLabel                         <<|>
                Colon <$ pToken ":"                          <<|>
                Comma <$ pToken ","                          <<|>
                Hash <$ pToken "#"                           <<|>
                CAnd <$ pToken "&&"                          <<|>
                COr <$ pToken "||"                           <<|>

                LRound <$ pToken "("                         <<|>
                RRound <$ pToken ")"                         <<|>
                LCurly <$ pToken "{"                         <<|>
                RCurly <$ pToken "}"                         <<|>
                RSquare <$ pToken "]" -- Other square bracket is parsed in parseString

parseTokens :: Parser [Token]
parseTokens = pMany parseToken

execParseTokens :: String -> ([Token], [Error LineColPos])
execParseTokens = execParser parseTokens
