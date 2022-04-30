{-# LANGUAGE LambdaCase #-}
module GLuaFixer.BadSequenceFinder (sequenceWarnings, checkFromString) where


import GLua.AG.Token
import GLua.PSParser
import GLuaFixer.LintMessage
import GLuaFixer.LintSettings
import Text.Parsec
import qualified GLua.PSLexer as PSL

-- | Satisfy for normal tokens
pTSatisfy :: (Token -> Bool) -> AParser MToken
pTSatisfy f = pMSatisfy f'
 where
    f' :: MToken -> Bool
    f' (MToken _ t) = f t

-- | Parse an identifier
ident :: String -> AParser MToken
ident s = pMSatisfy isIdent
 where
    isIdent :: MToken -> Bool
    isIdent (MToken _ (Identifier s')) = s == s'
    isIdent _ = False

-- | Parse any kind of whitespace
whitespace :: AParser MToken
whitespace = pTSatisfy isWhitespace

-- | Whether a token is whitespace
isWhitespace :: Token -> Bool
isWhitespace (Whitespace _) = True
isWhitespace _ = False

-- | Whether a token consists of spaces
isSpaces :: Token -> Bool
isSpaces (Whitespace str) = all (== ' ') str
isSpaces _ = False

-- | Parse anything that isn't whitespace
notWhitespace :: AParser MToken
notWhitespace = pMSatisfy isNotWhitespace
 where
    isNotWhitespace :: MToken -> Bool
    isNotWhitespace (MToken _ (Whitespace _)) = False
    isNotWhitespace _ = True

-- | Warnings for deprecated library functions
libraryWarnings :: String -> AParser String -> AParser String
libraryWarnings s p = do
    ident s
    pMTok Dot

    p

-- | Warnings for the ai library
aiWarnings :: AParser String
aiWarnings = libraryWarnings "ai" $
    "The function is broken" <$ ident "GetScheduleID" <|>
    "The function is broken" <$ ident "GetTaskID"

-- | Warnings for the math library
mathWarnings :: AParser String
mathWarnings = libraryWarnings "math" $
    "Use math.Distance instead" <$ ident "Dist" <|>
    "Use math.fmod instead" <$ ident "mod"


-- | Warnings for the spawnmenu library
spawnmenuWarnings :: AParser String
spawnmenuWarnings = libraryWarnings "spawnmenu" $
    "Use spawnmenu.SaveToTextFiles instead" <$ ident "DoSaveToTextFiles" <|>
    "Use spawnmenu.PopulateFromTextFiles instead" <$ ident "PopulateFromEngineTextFiles" <|>
    "The function is broken" <$ ident "SwitchToolTab"

-- | Warnings for the string library
stringWarnings :: AParser String
stringWarnings = libraryWarnings "string" $
    "Use either string.sub(str, index, index) or str[index]" <$ ident "GetChar" <|>
    "Use string.gmatch instead" <$ ident "gfind"

-- | Warnings for the surface library
surfaceWarnings :: AParser String
surfaceWarnings = libraryWarnings "surface" $
    "Use ScrH instead" <$ ident "ScreenHeight" <|>
    "Use ScrW instead" <$ ident "ScreenWidth"

-- | Warnings for the table library
tableWarnings :: AParser String
tableWarnings = libraryWarnings "table" $
    "Use ipairs or something instead" <$ (
        ident "FindNext" <|>
        ident "FindPrev" <|>
        ident "foreach"  <|>
        ident "ForEach"  <|>
        ident "foreachi"
    ) <|>
    "Use next instead" <$ (
        ident "GetFirstKey" <|>
        ident "GetFirstValue"
    ) <|>
    "Use #tbl instead"      <$ ident "GetLastKey"   <|>
    "Use tbl[#tbl] instead" <$ ident "GetLastValue" <|>
    "Use #tbl instead"      <$ ident "getn"


-- | Warnings for the timer library
timerWarnings :: AParser String
timerWarnings = libraryWarnings "timer" $
    "The function is broken" <$ ident "Check" <|>
    "Use timer.Remove instead" <$ ident "Destroy"

-- | Warnings for the umsg library
umsgWarnings :: AParser String
umsgWarnings = libraryWarnings "umsg" $
    "Use net messages." <$ ident "Start"

-- | Warnings for the util library
utilWarnings :: AParser String
utilWarnings = libraryWarnings "util" $
    "Use tobool, without the util bit" <$ ident "tobool" <|>
    "The function is broken" <$ ident "TraceEntityHull"

-- | Warnings for things to do with self
selfWarnings :: AParser String
selfWarnings = libraryWarnings "self" $
    "Use self:GetOwner() instead" <$ ident "Owner"

-- | Warnings for meta functions
metaFuncWarnings :: AParser String
metaFuncWarnings = do
    pMTok Colon

    -- CLuaLocomotion functions
    "Use :IsUsingLadder instead"                                     <$ ident "IsAscendingOrDescendingLadder"       <|>

      -- Panel functions
      "Use :GetPaintBackground instead"                                <$ ident "GetDrawBackground"                   <|>
      "Use :SetPaintBackground instead"                                <$ ident "SetDrawBackground"                   <|>
      "The function is broken"                                         <$ ident "AddText"                             <|>
      "Only used by deprecated Derma controls"                         <$ ident "PostMessage"                         <|>
      "Only used in deprecated Derma controls"                         <$ ident "SetActionFunction"                   <|>
      "Use :SetKeyboardInputEnabled instead"                           <$ ident "SetKeyBoardInputEnabled"             <|>
      "The function is broken"                                         <$ ident "SetPaintFunction"                    <|>
      "Use :SetTooltip instead, notice the lowercase fucking t"        <$ ident "SetToolTip"                          <|>
      "use :SetTooltipPanel instead, notice the lowercase fucking t"   <$ ident "SetToolTipPanel"                     <|>
      "Use :IsValid instead"                                           <$ ident "Valid"                               <|>

      -- Entity functions
      "Use :GetHitBoxBone instead, note the capital fucking B"         <$ ident "GetHitboxBone"                       <|>
      "Use :GetNWAngle instead"                                        <$ ident "GetNetworkedAngle"                   <|>
      "Use :GetNWBool instead"                                         <$ ident "GetNetworkedBool"                    <|>
      "Use :GetNWEntity instead"                                       <$ ident "GetNetworkedEntity"                  <|>
      "Use :GetNWFloat instead"                                        <$ ident "GetNetworkedFloat"                   <|>
      "Use :GetNWInt instead"                                          <$ ident "GetNetworkedInt"                     <|>
      "Use :GetNWString instead"                                       <$ ident "GetNetworkedString"                  <|>
      "Use :GetNWVarProxy instead"                                     <$ ident "GetNetworkedVarProxy"                <|>
      "Use :GetNWVarTable instead"                                     <$ ident "GetNetworkedVarTable"                <|>
      "Use :GetNWVector instead"                                       <$ ident "GetNetworkedVector"                  <|>
      "The function is broken"                                         <$ ident "GetWorkshopID"                       <|>
      "Use :SetNWAngle instead"                                        <$ ident "SetNetworkedAngle"                   <|>
      "Use :SetNWBool instead"                                         <$ ident "SetNetworkedBool"                    <|>
      "Use :SetNWEntity instead"                                       <$ ident "SetNetworkedEntity"                  <|>
      "Use :SetNWFloat instead"                                        <$ ident "SetNetworkedFloat"                   <|>
      "Use :SetNWInt instead"                                          <$ ident "SetNetworkedInt"                     <|>
      "Use :SetNWString instead"                                       <$ ident "SetNetworkedString"                  <|>
      "Use :SetNWVarProxy instead"                                     <$ ident "SetNetworkedVarProxy"                <|>
      "Use :SetNWVector instead"                                       <$ ident "SetNetworkedVector"                  <|>

      -- Player functions
      "Use :GetViewPunchAngles instead"                                <$ ident "GetPunchAngle"                       <|>

      -- Material functions
      "The function is broken"                                         <$ ident "SetShader"                           <|>

      -- Vector functions
      "Use :Dot instead"                                               <$ ident "DotProduct"


-- | Parser for all deprecated sequences of tokens
deprecatedSequence :: LintSettings -> AParser Issue
deprecatedSequence opts = if not (lint_deprecated opts) then parserZero else Deprecated <$> (

    -- Deprecated meta functions
    try metaFuncWarnings

    -- Library functions
    <|>try aiWarnings
    <|>try mathWarnings
    <|>try spawnmenuWarnings
    <|>try stringWarnings
    <|>try surfaceWarnings
    <|>try tableWarnings
    <|>try timerWarnings
    <|>try umsgWarnings
    <|>try utilWarnings
    <|>try selfWarnings

    -- Global functions
    <|> "Use collectgarbage(\"count\") instead"
        <$ ident "gcinfo"
    <|> "Use ConVar objects instead"
        <$ ident "GetConVarNumber"
    <|> "Use ConVar objects instead"
        <$ ident "GetConVarString"
    <|> "Use AddCSLuaFile in the file itself instead"
        <$ ident "IncludeCS"
    <|> "Use ScreenScale instead"
        <$ ident "SScale"
    <|> "Use IsUselessModel instead"
        <$ ident "UTIL_IsUselessModel"
    <|> "Use IsValid instead"
        <$ ident "ValidPanel"
    <|> "Use net messages."
        <$ ident "SendUserMessage"
    )

-- | Parser for all beginner mistakes
beginnerMistakeSequence :: LintSettings -> AParser Issue
beginnerMistakeSequence opts = if not (lint_beginnerMistakes opts) then parserZero else BeginnerMistake <$>
    (try ("There's little fucking reason to use ';' in the first place, don't use it twice in a row" <$ pMTok Semicolon <* pMTok Semicolon) <|>
    try ("The server already knows who sent the net message, use the first parameter of net.Receive" <$ do
        ident "net"
        pMTok Dot
        ident "WriteEntity"
        pMTok LRound
        optional whitespace
        ident "LocalPlayer"
        optional whitespace
        pMTok LRound
        optional whitespace
        pMTok RRound
        ) <|>
    try ("Jesus christ fuck off already" <$ do
        pMTok While
        whitespace
        pMTok TTrue
        whitespace
        pMTok Do
        whitespace
        pMTok End
        )
    )

whiteSpaceStyleSequence :: LintSettings -> AParser Issue
whiteSpaceStyleSequence opts = if not (lint_whitespaceStyle opts) then parserZero else WhitespaceStyle <$> (
    try ("Please put some whitespace after 'if'" <$ pMTok If <* notFollowedBy whitespace)            <|>
    try ("Please put some whitespace after 'elseif'" <$ pMTok Elseif <* notFollowedBy whitespace)    <|>
    try ("Please put some whitespace after 'while'" <$ pMTok While <* notFollowedBy whitespace)      <|>
    try ("Please put some whitespace after 'until'" <$ pMTok Until <* notFollowedBy whitespace)      <|>

    try ("Please put some whitespace after ')'" <$ do
        pMTok RRound
        pTSatisfy (\t -> not (isWhitespace t) && t `notElem` [Colon, RRound, LRound, LSquare, RSquare, LCurly, RCurly, Comma, Dot, Semicolon])
        ) <|>
    try ("Please put some whitespace before the operator" <$ do
        notWhitespace
        choice [pMTok Plus, pMTok Multiply, pMTok Divide, pMTok Modulus, pMTok TEq, pMTok TNEq, pMTok TCNEq, pMTok TLEQ, pMTok TGEQ, pMTok TLT, pMTok TGT, pMTok Equals, pMTok Concatenate, pMTok And, pMTok CAnd, pMTok Or, pMTok COr]
        ) <|>
    try ("Please put some whitespace after the operator" <$ do
        choice [pMTok Plus, pMTok Multiply, pMTok Divide, pMTok Modulus, pMTok TEq, pMTok TNEq, pMTok TCNEq, pMTok TLEQ, pMTok TGEQ, pMTok TLT, pMTok TGT, pMTok Equals, pMTok Concatenate, pMTok And, pMTok CAnd, pMTok Or, pMTok COr]
        notWhitespace
        )
    )

-- | Matches any token but the given one
pNotTToken :: Token -> AParser MToken
pNotTToken t = pTSatisfy (t /=)

-- | Warn about adding or removing spaces after an opening parenthesis. What it actually checks for
-- and wants the user to do depends on the prettyprint_spaceAfterParens and
-- prettyprint_spaceEmptyParens settings
spaceAfterParenthesis :: LintSettings -> AParser Issue
spaceAfterParenthesis settings
    | not (lint_spaceAfterParens settings) = parserZero
    | otherwise =
        case (prettyprint_spaceAfterParens settings, prettyprint_spaceEmptyParens settings) of
          (True, True) ->
            SpaceAfterParenthesis AddSpace <$ try (pMTok LRound >> notWhitespace)
          (False, False) ->
            SpaceAfterParenthesis RemoveSpace <$ try (pMTok LRound >> pTSatisfy isSpaces)
          (True, False) ->
            SpaceAfterParenthesis AddSpace <$ try (pMTok LRound >> notWhitespaceAndNotClose) <|>
            SpaceAfterParenthesis RemoveSpace <$ try (pMTok LRound >> pTSatisfy isSpaces >> pMTok RRound)
          (False, True) ->
            SpaceAfterParenthesis RemoveSpace <$ try (pMTok LRound >> pTSatisfy isSpaces >> pNotTToken RRound) <|>
            SpaceAfterParenthesis AddSpace <$ try (pMTok LRound >> pMTok RRound)
  where
    notWhitespaceAndNotClose :: AParser MToken
    notWhitespaceAndNotClose = pTSatisfy $ \case
      RRound -> False
      Whitespace _ -> False
      _ -> True

-- | Warn about adding or removing spaces before a closing parenthesis
spaceBeforeParenthesis :: LintSettings -> AParser Issue
spaceBeforeParenthesis settings
    | not (lint_spaceAfterParens settings) = parserZero
    | otherwise =
        case (prettyprint_spaceAfterParens settings, prettyprint_spaceEmptyParens settings) of
          (True, True) ->
            SpaceBeforeParenthesis AddSpace <$ try (notWhitespace >> pMTok RRound)
          (False, False) ->
            SpaceBeforeParenthesis RemoveSpace <$ try (pTSatisfy isSpaces >> pMTok RRound)
          (True, False) ->
            SpaceBeforeParenthesis AddSpace <$ try (notWhitespaceAndNotOpen >> pMTok RRound)
          (False, True) ->
            SpaceBeforeParenthesis RemoveSpace <$ try (pNotTToken LRound >> pTSatisfy isSpaces >> pMTok RRound)
  where
    notWhitespaceAndNotOpen :: AParser MToken
    notWhitespaceAndNotOpen = pTSatisfy $ \case
      LRound -> False
      Whitespace _ -> False
      _ -> True

-- | Warn about adding or removing spaces after an opening brace (`}`). What it actually checks for
-- and wants the user to do depends on the prettyprint_spaceAfterBraces and
-- prettyprint_spaceEmptyBraces settings
spaceAfterBraces :: LintSettings -> AParser Issue
spaceAfterBraces settings
    | not (lint_spaceAfterBraces settings) = parserZero
    | otherwise =
        case (prettyprint_spaceAfterBraces settings, prettyprint_spaceEmptyBraces settings) of
          (True, True) ->
            SpaceAfterBrace AddSpace <$ try (pMTok LCurly >> notWhitespace)
          (False, False) ->
            SpaceAfterBrace RemoveSpace <$ try (pMTok LCurly >> pTSatisfy isSpaces)
          (True, False) ->
            SpaceAfterBrace AddSpace <$ try (pMTok LCurly >> notWhitespaceAndNotClose) <|>
            SpaceAfterBrace RemoveSpace <$ try (pMTok LCurly >> pTSatisfy isSpaces >> pMTok RCurly)
          (False, True) ->
            SpaceAfterBrace RemoveSpace <$ try (pMTok LCurly >> pTSatisfy isSpaces >> pNotTToken RCurly) <|>
            SpaceAfterBrace AddSpace <$ try (pMTok LCurly >> pMTok RCurly)
  where
    notWhitespaceAndNotClose :: AParser MToken
    notWhitespaceAndNotClose = pTSatisfy $ \case
      RCurly -> False
      Whitespace _ -> False
      _ -> True

-- | Warn about adding or removing spaces before a closing brace (`}`)
spaceBeforeBraces :: LintSettings -> AParser Issue
spaceBeforeBraces settings
    | not (lint_spaceAfterBraces settings) = parserZero
    | otherwise =
        case (prettyprint_spaceAfterBraces  settings, prettyprint_spaceEmptyBraces settings) of
          (True, True) ->
            SpaceBeforeBrace AddSpace <$ try (notWhitespace >> pMTok RCurly)
          (False, False) ->
            SpaceBeforeBrace RemoveSpace <$ try (pTSatisfy isSpaces >> pMTok RCurly)
          (True, False) ->
            SpaceBeforeBrace AddSpace <$ try (notWhitespaceAndNotOpen >> pMTok RCurly)
          (False, True) ->
            SpaceBeforeBrace RemoveSpace <$ try (pNotTToken LCurly >> pTSatisfy isSpaces >> pMTok RCurly)
  where
    notWhitespaceAndNotOpen :: AParser MToken
    notWhitespaceAndNotOpen = pTSatisfy $ \case
      LCurly -> False
      Whitespace _ -> False
      _ -> True

-- | Warn about adding or removing spaces after an opening bracket (`[`). What it actually checks
-- for and wants the user to do depends on the prettyprint_spaceAfterBracket
spaceAfterBrackets :: LintSettings -> AParser Issue
spaceAfterBrackets settings
    | not (lint_spaceAfterBrackets settings) = parserZero
    | prettyprint_spaceAfterBrackets settings =
      SpaceAfterBracket AddSpace <$ try (pMTok LSquare >> notWhitespace)
    | otherwise =
      SpaceAfterBracket RemoveSpace <$ try (pMTok LSquare >> pTSatisfy isSpaces)

-- | Warn about adding or removing spaces before a closing bracket (`]`)
spaceBeforeBrackets :: LintSettings -> AParser Issue
spaceBeforeBrackets settings
    | not (lint_spaceAfterBrackets settings) = parserZero
    | prettyprint_spaceAfterBrackets settings =
      SpaceAfterBracket AddSpace <$ try (notWhitespace >> pMTok RSquare)
    | otherwise =
      SpaceAfterBracket RemoveSpace <$ try (pTSatisfy isSpaces >> pMTok RSquare)

-- | Parser for all profanity
profanitySequence :: LintSettings -> AParser Issue
profanitySequence opts = if not (lint_profanity opts) then parserZero else Profanity <$ (
    ident "anus"                    <|>
    ident "bitch"                   <|>
    ident "cock"                    <|>
    ident "cocks"                   <|>
    ident "cunt"                    <|>
    ident "dick"                    <|>
    ident "dicks"                   <|>
    ident "fuck"                    <|>
    ident "fucking"                 <|>
    ident "goddamnit"               <|>
    ident "knob"                    <|>
    ident "knobs"                   <|>
    ident "motherfucker"            <|>
    ident "nigger"                  <|>
    ident "niggers"                 <|>
    ident "niggertits"              <|>
    ident "nipple"                  <|>
    ident "shit"
    )

-- | Parses for any bad sequence
badSequence :: LintSettings -> AParser Issue
badSequence opts =
    deprecatedSequence opts <|>
    profanitySequence opts <|>
    beginnerMistakeSequence opts <|>
    whiteSpaceStyleSequence opts <|>

    spaceAfterParenthesis opts <|>
    spaceBeforeParenthesis opts <|>

    spaceAfterBraces opts <|>
    spaceBeforeBraces opts <|>

    spaceAfterBrackets opts <|>
    spaceBeforeBrackets opts

-- | Creates a warning for a certain sequence at any position
badSequenceWarning :: Region -> Issue -> [FilePath -> LintMessage] -> [FilePath -> LintMessage]
badSequenceWarning pos message = (:) (LintMessage LintWarning pos message)

-- | Searches for all the bad sequences
badSequenceParser :: LintSettings -> AParser [String -> LintMessage]
badSequenceParser opts =
    -- A bad sequence
    annotated badSequenceWarning (badSequence opts) <*> badSequenceParser opts <|>
    -- Continue searching
    anyToken *> badSequenceParser opts <|>
    -- end of input
    return []

-- | Returns all the warnings for a lexicon
sequenceWarnings :: LintSettings -> [MToken] -> [String -> LintMessage]
sequenceWarnings opts mts = case execAParser "source.lua" (badSequenceParser opts) mts of
    Left _ -> error "[Error] line 1, column 1: Sequence finding error! Report an issue!"
    Right warnings -> warnings

-- | Helper function: check from string
checkFromString :: AParser a -> String -> Either ParseError a
checkFromString p inp = do
    lexed <- PSL.execParseTokens inp
    execAParser "source.lua" p lexed
