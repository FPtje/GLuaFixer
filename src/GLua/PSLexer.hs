{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             BangPatterns #-}

module GLua.PSLexer where

import qualified GLua.AG.Token as T

import Text.Megaparsec
import Text.Megaparsec.String
import Text.ParserCombinators.UU.BasicInstances(LineColPos(..))

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

-- | Get the source position
pPos :: Parser LineColPos
pPos = sp2lcp <$> getPosition

-- | Underscore character
underscore :: Parser Char
underscore = char '_'

-- | Parse the string until the end. Used in parseLineComment among others.
pUntilEnd :: Parser String
pUntilEnd = manyTill anyChar (lookAhead (eol <|> const "\n" <$> eof))

-- | Whitespace parser.
parseWhitespace :: Parser String
parseWhitespace = some spaceChar <?> "whitespace"

-- | An escaped character of a single line string
-- Takes the delimiter of the string as parameter
escapedStringChar :: Char -> Parser String
escapedStringChar delim = (\c -> ['\\', c]) <$ char '\\' <*> noneOf ['\n'] <|> (:[]) <$> noneOf ['\n', delim]

-- | The start of a nested string
-- Returns the amount of =-signs at the start of the nested string
startNestedString :: Parser String
startNestedString = do
    char '['
    depth <- many (char '=')
    char '['
    return depth

-- | Parses a multiline string
-- returns the amount of =-signs in the string delimiter and the contents
nestedString :: String -> Parser String
nestedString depth = do
    let endStr = "]" ++ depth ++ "]"
    manyTill anyChar (try $ string endStr)

-- | Parse Lua style comments
parseLuaComment :: Parser T.Token
parseLuaComment = do
    string "--"
    -- When a nested comment is started, it must be finished
    startNested <- optional (try startNestedString)

    case startNested of
        Nothing -> T.DashComment <$> pUntilEnd
        Just depth -> do
            contents <- nestedString depth
            return $ T.DashBlockComment (length depth) contents


-- | Parse C-Style comments
parseCComment :: Parser T.Token
parseCComment = do
    char '/'
    try (T.SlashComment <$ char '/' <*> pUntilEnd) <|>
     T.SlashBlockComment <$ char '*' <*> manyTill anyChar (try (string "*/") <|> const "\n" <$> eof)

-- | Parse any kind of comment.
parseComment :: Parser T.Token
parseComment = parseLuaComment <|> parseCComment <?> "Comment"

-- | Convert the result of the nestedString parser to a MultiLine string
nestedStringToMLString :: String -> String -> T.Token
nestedStringToMLString depth contents = T.MLString (showChar '[' . showString depth . showChar '[' . showString contents . showChar ']' . showString depth . showChar ']' $ "")

-- | Parse single line strings e.g. "sdf", 'werf'.
parseLineString :: Char -> Parser String
parseLineString c = concat <$ char c <*> many (escapedStringChar c) <* char c

-- | Single and multiline strings.
parseString :: Parser T.Token
parseString = T.SQString <$> parseLineString '\'' <|>
              T.DQString <$> parseLineString '"' <|> do
                depth <- startNestedString
                nestedStringToMLString depth <$> nestedString depth <?> "String"

-- | Parse any kind of number.
-- Except for numbers that start with a '.'. That's handled by parseDots to solve ambiguity.
parseNumber :: Parser T.Token
parseNumber = T.TNumber <$> ((++) <$> (pHexadecimal <|> pNumber) <*> option "" parseNumberSuffix) <?> "Number"
    where
        pNumber :: Parser String
        pNumber = (++) <$> some digitChar <*> option "" pDecimal

        pDecimal :: Parser String
        pDecimal = (:) <$> char '.' <*> many digitChar

        pHexadecimal :: Parser String
        pHexadecimal = (++) <$> try (string "0x") <*> ((++) <$> some pHex <*> option "" pDecimal)

        pHex :: Parser Char
        pHex = digitChar <|> oneOf ['a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F']

-- Parse the suffix of a number
parseNumberSuffix :: Parser String
parseNumberSuffix = (\e s d -> e : s ++ d) <$> oneOf ['e', 'E', 'p', 'P']
            <*> option "" (string "+" <|> string "-")
            <*> some digitChar

-- | Parse either a keyword or an identifier that starts with that keyword
parseKeyword :: T.Token -> String -> Parser T.Token
parseKeyword tok str = try (do
    string str
    (\s -> T.Identifier (str ++ s)) <$> some (underscore <|> alphaNumChar) <|>
        return tok
    <?> "Keyword " ++ str)

-- | Parse just an identifier.
parseIdentifier :: Parser String
parseIdentifier = (:) <$> (letterChar <|> underscore) <*> many allowed <?> "Identifier"
    where
        allowed = letterChar <|> digitChar <|> underscore

-- | Parse a label.
parseLabel :: Parser String
parseLabel = string "::" *> parseIdentifier <* string "::" <?> "Label"

-- | Parse anything to do with dots. Indexaction (.), concatenation (..), varargs (...) or numbers that start with a dot
parseDots :: Parser T.Token
parseDots = do
    char '.' -- first .
    try (do
        char '.' -- second .
        try (T.VarArg <$ char '.') <|> -- third .
            return T.Concatenate) <|>

        -- try to parse a number that starts with a .
        try (do
            nums <- some digitChar
            suffix <- option "" parseNumberSuffix
            return $ T.TNumber ('.' : (nums ++ suffix))
            ) <|>


        return T.Dot

-- | Parse any kind of token.
parseToken :: Parser T.Token
parseToken =
                T.Whitespace <$> parseWhitespace                  <|>
                -- Constants
                try parseString                                 <|>
                parseNumber                                     <|>
                parseKeyword T.TTrue "true"                       <|>
                parseKeyword T.TFalse "false"                     <|>
                parseKeyword T.Nil "nil"                          <|>
                parseKeyword T.Not "not"                          <|>
                parseKeyword T.And "and"                          <|>
                parseKeyword T.Or "or"                            <|>
                parseKeyword T.Function "function"                <|>
                parseKeyword T.Local "local"                      <|>
                parseKeyword T.If "if"                            <|>
                parseKeyword T.Then "then"                        <|>
                parseKeyword T.Elseif "elseif"                    <|>
                parseKeyword T.Else "else"                        <|>
                parseKeyword T.For "for"                          <|>
                parseKeyword T.In "in"                            <|>
                parseKeyword T.Do "do"                            <|>
                parseKeyword T.While "while"                      <|>
                parseKeyword T.Until "until"                      <|>
                parseKeyword T.Repeat "repeat"                    <|>
                parseKeyword T.Continue "continue"                <|>
                parseKeyword T.Break "break"                      <|>
                parseKeyword T.Return "return"                    <|>
                parseKeyword T.End "end"                          <|>

                T.Identifier <$> parseIdentifier                  <|>

                T.Semicolon  <$ char ';'                          <|>
                parseDots                                       <|>

                try parseComment                                <|>

                -- Operators
                T.Plus <$ string "+"                              <|>
                T.Minus <$ string "-"                             <|>
                T.Multiply <$ string "*"                          <|>
                T.Divide <$ string "/"                            <|>
                T.Modulus <$ string "%"                           <|>
                T.Power <$ string "^"                             <|>
                T.TEq <$ try (string "==")                        <|>
                T.Equals <$ string "="                            <|>
                T.TNEq <$ string "~="                             <|>
                T.TCNEq <$ try (string "!=")                      <|>
                T.CNot <$ string "!"                              <|>
                T.TLEQ <$ try (string "<=")                       <|>
                T.TLT <$ string "<"                               <|>
                T.TGEQ <$ try (string ">=")                       <|>
                T.TGT <$ string ">"                               <|>
                T.Label <$> try parseLabel                        <|>
                T.Colon <$ string ":"                             <|>
                T.Comma <$ string ","                             <|>
                T.Hash <$ string "#"                              <|>
                T.CAnd <$ string "&&"                             <|>
                T.COr <$ string "||"                              <|>

                T.LRound <$ char '('                              <|>
                T.RRound <$ char ')'                              <|>
                T.LCurly <$ char '{'                              <|>
                T.RCurly <$ char '}'                              <|>
                T.LSquare <$ char '['                             <|>
                T.RSquare <$ char ']'


-- | A thing of which the region is to be parsed
annotated :: (T.Region -> a -> b) -> Parser a -> Parser b
annotated f p = (\s t e -> f (T.Region s e) t) <$> pPos <*> p <*> pPos

-- | parse located MToken
parseMToken :: Parser T.MToken
parseMToken =
  do
    start <- pPos
    tok <- parseToken
    end <- pPos

    return $ T.MToken (T.Region start end) tok

-- | Parse a list of tokens and turn them into MTokens.
parseTokens :: Parser [T.MToken]
parseTokens = many parseMToken

-- | Parse the potential #!comment on the first line
-- Lua ignores the first line if it starts with #
parseHashBang :: Parser String
parseHashBang = option "" (char '#' *> pUntilEnd)

-- | Parse a string into MTokens.
execParseTokens :: String -> Either (ParseError Char Dec) [T.MToken]
execParseTokens = parse (parseHashBang *> parseTokens <* eof) "input"
