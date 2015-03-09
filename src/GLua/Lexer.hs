{-# LANGUAGE FlexibleInstances,
            TypeSynonymInstances,
            MultiParamTypeClasses,
            Rank2Types, FlexibleContexts, NoMonomorphismRestriction,
            CPP #-}

-- | Lex GLua into MTokens
module GLua.Lexer where

import GLua.TokenTypes

import Data.List

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived

-- | String parser that maintains positions.
type LParser a = P (Str Char String LineColPos) a

-- | Whitespace parser.
parseWhitespace :: LParser String
parseWhitespace = pMunch (`elem` " \r\n\t")

-- | Blanco parser. Parses anything. Used in parsing comments.
parseAnyChar :: LParser Char
parseAnyChar = pSatisfy (const True) (Insertion "Any character" 'y' 5)

-- | Parses a C-style block comment.
parseCBlockComment :: LParser String
parseCBlockComment = const "" <$> pToken "*/" <<|>
            (:) <$> parseAnyChar <*> parseCBlockComment

-- | Try to parse a block comment.
-- Might actually return a single line Dash comment, because for example
-- the following line is a line comment, rather than a block comment
--[===== <- missing the last '[' bracket.
parseBlockComment :: LParser Token
parseBlockComment = pToken "[" *> nested 0
    where
        -- The amount of =-signs in the string delimiter is n
        nested :: Int -> LParser Token
        nested n = pToken "=" *> nested (n + 1) <<|>
                   DashBlockComment n <$ pToken "[" <*> restString n <<|>
                   lineComment n <$> pUntilEnd

        -- Turns out we were describing a line comment all along, cheeky bastard!
        -- (the last [ of the block comment start token is missing)
        lineComment :: Int -> String -> Token
        lineComment n str = DashComment $ '[' : replicate n '=' ++ str

        -- Right-recursive grammar. This part searches for the rest of the string until it finds the ]=^n] token
        restString :: Int -> LParser String
        restString n = const "" <$> pToken ("]" ++ replicate n '=' ++ "]") <<|>
                       (:) <$> parseAnyChar <*> restString n

-- | Parse the string until the end. Used in parseLineComment among others.
pUntilEnd :: LParser String
pUntilEnd = pMunch (\c -> c /= '\n' && c /= '\r')

-- | A comment that spans until the end of the line.
parseLineComment :: String -> LParser String
parseLineComment prefix = flip const <$> pToken prefix <*> pUntilEnd

-- | Parses a multiline string except for its first character (e.g. =[ string ]=])
-- This is because the first [ could also just be parsed as a square bracket.
nestedString :: LParser String
nestedString = nested 0
    where
        -- The amount of =-signs in the string delimiter is n
        nested :: Int -> LParser String
        nested n = (\str -> "=" ++ str) <$ pToken "=" <*> nested (n + 1) <<|>
                   ('[' :) <$ pToken "[" <*> restString n

        -- Right-recursive grammar. This part searches for the rest of the string until it finds the ]=^n] token
        restString :: Int -> LParser String
        restString n = pToken ("]" ++ replicate n '=' ++ "]") <<|>
                       (:) <$> parseAnyChar <*> restString n

-- | Parse any kind of comment.
parseComment :: LParser Token
parseComment =  pToken "--" <**> -- Dash block comment and dash comment both start with "--"
                    (const <$> (parseBlockComment <<|> DashComment <$> pUntilEnd)) <<|>
                pToken "/" <**>
                    (const <$>
                        (SlashBlockComment <$ pToken "*" <*> parseCBlockComment <<|>
                        SlashComment <$> parseLineComment "/" <<|>
                        pReturn Divide -- The /-sign is here because it also starts with '/'
                        ))

-- | Parse single line strings e.g. "sdf", 'werf'.
parseLineString :: Char -> LParser String
parseLineString c = pSym c *> innerString
    where
        innerString :: LParser String
        innerString = pSym '\\' <**> -- Escaped character in string always starts with backslash
                         ((\c str esc -> esc : c : str) <$> parseAnyChar <*> innerString) <<|>
                      const "" <$> pSym c <<|> -- the end of the string
                      (:) <$> pNoNewline <*> innerString -- the next character in the string

        pNoNewline :: LParser Char
        pNoNewline = pSatisfy (/= '\n') (Insertion "Anything but a newline" c 5)


-- | Single and multiline strings.
parseString :: LParser Token
parseString = DQString <$> parseLineString '"' <<|>
              SQString <$> parseLineString '\'' <<|>
              -- Parse either a multiline string or just a bracket.
              -- Placed here because they have the first token '[' in common
              pSym '[' <**> ((\_ -> MLString . (:) '[') <$$> nestedString <<|>
                        const <$> pReturn LSquare)

-- | Parse any kind of number.
parseNumber :: LParser Token
parseNumber = TNumber <$> ((++) <$> (pHexadecimal <<|> pNumber) <*> opt pSuffix "")

    where
        pNumber :: LParser String
        pNumber = (++) <$> pSome pDigit <*> opt ((:) <$> pSym '.' <*> pSome pDigit) ""

        pHexadecimal :: LParser String
        pHexadecimal = (++) <$> pToken "0x" <*> pSome pHex

        pHex :: LParser Char
        pHex = pDigit <<|> pSym 'a' <<|> pSym 'b' <<|> pSym 'c' <<|> pSym 'd' <<|> pSym 'e' <<|> pSym 'f'
                      <<|> pSym 'A' <<|> pSym 'B' <<|> pSym 'C' <<|> pSym 'D' <<|> pSym 'E' <<|> pSym 'F'

        pSuffix :: LParser String
        pSuffix = (\e s d -> e : s ++ d) <$> (pSym 'e' <<|> pSym 'E' <<|> pSym 'p' <<|> pSym 'P')
                    <*> opt (pToken "+" <<|> pToken "-") ""
                    <*> pSome pDigit

-- | Parse a keyword. Note: It must really a key/word/! This parser makes sure to return an identifier when
-- it's actually an identifier that starts with that keyword.
parseKeyword :: Token -> String -> LParser Token
parseKeyword tok word = pToken word <**>
                            ((\k -> Identifier . (++) k) <$$> pSome allowed <<|> const <$> pReturn tok)
    where
        allowed = pSym '_' <<|> pLetter <<|> pDigit

-- | Parse just an identifier.
parseIdentifier :: LParser String
parseIdentifier = (:)  <$> (pSym '_' <<|> pLetter) <*> pMany allowed
    where
        allowed = pSym '_' <<|> pLetter <<|> pDigit

-- | Parse a label.
parseLabel :: LParser String
parseLabel = pToken "::" *> parseIdentifier <* pToken "::"

-- | Parse anything to do with dots. Indexaction (.), concatenation (..) or varargs (...)
parseDots :: LParser Token
parseDots = pToken "." <**> ( -- A dot means it's either a VarArg (...), concatenation (..) or just a dot (.)
                const <$> (pToken "." <**> (
                    const VarArg <$ pToken "." <<|>
                    const <$> pReturn Concatenate
                )) <<|>
                const <$> pReturn Dot
                )

-- | Parse any kind of token.
parseToken :: LParser Token
parseToken =    parseComment                                 <<|>

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
                Multiply <$ pToken "*"                       <<|>
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

-- | Parse a list of tokens and turn them into MTokens.
parseTokens :: LParser [MToken]
parseTokens = pMany (MToken <$> pPos <*> parseToken <* parseWhitespace)

-- | Parse a string into MTokens. Also returns parse errors.
execParseTokens :: String -> ([MToken], [Error LineColPos])
execParseTokens = parse ((,) <$ parseWhitespace <*> parseTokens <*> pErrors <* pEnd) . createStr (LineColPos 0 0 0)

