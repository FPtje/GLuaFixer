{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}

-- | Lex GLua into MTokens
module GLua.Lexer where

import GLua.AG.Token ( Region(..), MToken(..), Token(..) )

import Data.Char (ord)
import Text.ParserCombinators.UU
    ( P,
      pEnd,
      pErrors,
      (<**>),
      micro,
      pPos,
      parse,
      (<$$>),
      pMany,
      pMaybe,
      pReturn,
      pSome,
      ExtAlternative(opt, (<<|>)) )
import Text.ParserCombinators.UU.Utils ( pDigit )
import Text.ParserCombinators.UU.BasicInstances
    ( Error,
      LineColPos(..),
      Str,
      createStr,
      pMunch,
      pSatisfy,
      pSym,
      pToken,
      Insertion(Insertion) )

-- | String parser that maintains positions.
type LParser a = P (Str Char String LineColPos) a

-- | Whitespace parser that requires at least one whitespace character
parseWhitespace :: LParser String
parseWhitespace = pSome $ pSatisfy (`elem` " \r\n\t") (Insertion "Whitespace" ' ' 5)

-- | Whitespace parser that requires 0 or more whitespace characters
parseOptionalWhitespace :: LParser String
parseOptionalWhitespace = pMany $ pSatisfy (`elem` " \r\n\t") (Insertion "Whitespace" ' ' 5)

-- | Blanco parser. Parses anything. Used in parsing comments.
parseAnyChar :: LParser Char
parseAnyChar = pSatisfy (const True) (Insertion "Any character" 'y' 5)

-- See luajit's src/lj_char.c and src/lj_char.h
pIdentifierCharacter :: LParser Char
pIdentifierCharacter = pSatisfy validChar (Insertion "Identifying character (letter, number, emoji)" 'a' 5)
    where
        validChar :: Char -> Bool
        validChar c =
            between c '0' '9' ||
            between c 'A' 'Z' ||
            c == '_'          ||
            between c 'a' 'z' ||
            ord c >= 128


pNonDigitIdentifierCharacter :: LParser Char
pNonDigitIdentifierCharacter = pSatisfy validChar (Insertion "Identifying character (letter, emoji)" 'a' 5)
    where
        validChar :: Char -> Bool
        validChar c =
            between c 'A' 'Z' ||
            c == '_'          ||
            between c 'a' 'z' ||
            ord c >= 128

between :: Char -> Char -> Char -> Bool
between c left right = c >= left && c <= right

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
                         ((\c' str esc -> esc : c' ++ str) <$> escapeSequence <*> innerString) <<|>
                      const "" <$> pSym c <<|> -- the end of the string
                      (:) <$> pNoNewline <*> innerString -- the next character in the string

        escapeSequence :: LParser String
        escapeSequence = (:) <$> pSym 'z' <*> parseOptionalWhitespace <<|> (:[]) <$> parseAnyChar

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
parseNumber = TNumber <$> ((++) <$> (pZeroPrefixedNumber <<|> pNumber) <*> (pLLULL <<|> opt parseNumberSuffix ""))
    where
        -- Numbers starting with 0 can be regular numbers, hexadecimals or binary
        pZeroPrefixedNumber :: LParser String
        pZeroPrefixedNumber =
            pSym '0' <**> (
                (\hex _0 -> _0 : hex) <$> pHexadecimal <<|>
                (\bin _0 -> _0 : bin) <$> pBinary <<|>
                (\digits _0 -> _0 : digits) <$> (pDecimal <<|> pNumber) <<|>
                pure (:[])
            )

        pNumber :: LParser String
        pNumber = (++) <$> pSome pDigit <*> opt pDecimal ""

        pDecimal :: LParser String
        pDecimal = (:) <$> pSym '.' <*> pMany pDigit

        pHexadecimal :: LParser String
        pHexadecimal = (:) <$> (pSym 'x' <<|> pSym 'X') <*> ((++) <$> pSome pHex <*> opt pDecimal "")

        pBinary :: LParser String
        pBinary = (:) <$> (pSym 'b' <<|> pSym 'B') <*> ((++) <$> pSome pBin <*> opt pDecimal "")

        pHex :: LParser Char
        pHex = pDigit <<|> pSym 'a' <<|> pSym 'b' <<|> pSym 'c' <<|> pSym 'd' <<|> pSym 'e' <<|> pSym 'f'
                      <<|> pSym 'A' <<|> pSym 'B' <<|> pSym 'C' <<|> pSym 'D' <<|> pSym 'E' <<|> pSym 'F'

        pBin :: LParser Char
        pBin = pSym '0' <<|> pSym '1'

        -- LL/ULL suffix of a number, making it signed/unsigned int64 respectively
        -- http://luajit.org/ext_ffi_api.html#literals
        pLLULL :: LParser String
        pLLULL = pULL <<|> pLL

        pLL :: LParser String
        pLL = "LL" <$ (pSym 'L' <<|> pSym 'l') <* (pSym 'L' <<|> pSym 'l')

        pULL :: LParser String
        pULL = "ULL" <$ (pSym 'U' <<|> pSym 'u') <* pLL

-- Parse the suffix of a number
parseNumberSuffix :: LParser String
parseNumberSuffix = imaginary <<|> extension
    where
        imaginary = (:[]) <$> (pSym 'i' <<|> pSym 'I')

        extension = (\e s d -> e : s ++ d) <$> (pSym 'e' <<|> pSym 'E' <<|> pSym 'p' <<|> pSym 'P')
            <*> opt (pToken "+" <<|> pToken "-") ""
            <*> pSome pDigit

-- | Parse a keyword. Note: It must really a key/word/! This parser makes sure to return an identifier when
-- it's actually an identifier that starts with that keyword.
parseKeyword :: Token -> String -> LParser Token
parseKeyword tok word = pToken word <**>
                            ((\k -> Identifier . (++) k) <$$> pSome pIdentifierCharacter <<|> const <$> pReturn tok)

-- | Parse just an identifier.
parseIdentifier :: LParser String
parseIdentifier = (:) <$> pNonDigitIdentifierCharacter <*> pMany pIdentifierCharacter

-- | Parse a label.
parseLabel :: LParser String
parseLabel = pToken "::" *> (pMaybe parseWhitespace *> parseIdentifier <* pMaybe parseWhitespace) <* pToken "::"

-- | Parse anything to do with dots. Indexaction (.), concatenation (..) or varargs (...)
parseDots :: LParser Token
parseDots = pToken "." <**> ( -- A dot means it's either a VarArg (...), concatenation (..) or just a dot (.)
                const <$> (pToken "." <**> (
                    const VarArg <$ pToken "." <<|>
                    const <$> pReturn Concatenate
                )) <<|>
                (\ds sfx dot -> TNumber $ dot ++ ds ++ sfx) <$> pSome pDigit <*> opt parseNumberSuffix "" <<|>
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
                Hash <$ pToken "#"  `micro` 10               <<|> -- Add micro cost to prevent conflict with parseHashBang
                CAnd <$ pToken "&&"                          <<|>
                COr <$ pToken "||"                           <<|>

                LRound <$ pToken "("                         <<|>
                RRound <$ pToken ")"                         <<|>
                LCurly <$ pToken "{"                         <<|>
                RCurly <$ pToken "}"                         <<|>
                -- Other square bracket is parsed in parseString
                RSquare <$ pToken "]"                        <<|>
                Whitespace <$> parseWhitespace


-- | A thing of which the region is to be parsed
annotated :: (Region -> a -> b) -> LParser a -> LParser b
annotated f p = (\s t e -> f (Region s e) t) <$> pPos <*> p <*> pPos

-- | parse located MToken
parseMToken :: LParser MToken
parseMToken = annotated MToken parseToken

-- | Parse a list of tokens and turn them into MTokens.
parseTokens :: LParser [MToken]
parseTokens = pMany parseMToken

-- | Parse the potential #!comment on the first line
-- Lua ignores the first line if it starts with #
parseHashBang :: LParser String
parseHashBang = opt (pToken "#" <* pUntilEnd) ""

-- | Lex a string with a given lexer
lexFromString :: LParser a -> String -> (a, [Error LineColPos])
lexFromString p = parse ((,) <$> p <*> pErrors <* pEnd) . createStr (LineColPos 0 0 0)

-- | Parse a string into MTokens. Also returns parse errors.
execParseTokens :: String -> ([MToken], [Error LineColPos])
execParseTokens = parse ((,) <$ parseHashBang <*> parseTokens <*> pErrors <* pEnd) . createStr (LineColPos 0 0 0)
