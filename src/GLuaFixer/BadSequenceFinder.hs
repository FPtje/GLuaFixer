module GLuaFixer.BadSequenceFinder (sequenceWarnings, checkFromString) where


import GLua.AG.PrettyPrint
import GLua.AG.Token
import GLua.PSParser
import GLuaFixer.LintSettings
import Text.Parsec
import Text.ParserCombinators.UU.BasicInstances(LineColPos(..))
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
    const "The function is broken" <$> ident "GetScheduleID" <|>
    const "The function is broken" <$> ident "GetTaskID"

-- | Warnings for the math library
mathWarnings :: AParser String
mathWarnings = libraryWarnings "math" $
    const "Use math.Distance instead" <$> ident "Dist" <|>
    const "Use math.fmod instead" <$> ident "mod"


-- | Warnings for the spawnmenu library
spawnmenuWarnings :: AParser String
spawnmenuWarnings = libraryWarnings "spawnmenu" $
    const "Use spawnmenu.SaveToTextFiles instead" <$> ident "DoSaveToTextFiles" <|>
    const "Use spawnmenu.PopulateFromTextFiles instead" <$> ident "PopulateFromEngineTextFiles" <|>
    const "The function is broken" <$> ident "SwitchToolTab"

-- | Warnings for the string library
stringWarnings :: AParser String
stringWarnings = libraryWarnings "string" $
    const "Use either string.sub(str, index, index) or str[index]" <$> ident "GetChar" <|>
    const "Use string.gmatch instead" <$> ident "gfind"

-- | Warnings for the surface library
surfaceWarnings :: AParser String
surfaceWarnings = libraryWarnings "libraryWarnings" $
    const "Use ScrH instead" <$> ident "ScreenHeight" <|>
    const "Use ScrW instead" <$> ident "ScreenWidth"

-- | Warnings for the table library
tableWarnings :: AParser String
tableWarnings = libraryWarnings "table" $
    const "Use ipairs or something instead" <$> (
        ident "FindNext" <|>
        ident "FindPrev" <|>
        ident "foreach"  <|>
        ident "ForEach"  <|>
        ident "foreachi"
    ) <|>
    const "Use next instead" <$> (
        ident "GetFirstKey" <|>
        ident "GetFirstValue"
    ) <|>
    const "Use #tbl instead"      <$> ident "GetLastKey"   <|>
    const "Use tbl[#tbl] instead" <$> ident "GetLastValue" <|>
    const "Use #tbl instead"      <$> ident "getn"


-- | Warnings for the timer library
timerWarnings :: AParser String
timerWarnings = libraryWarnings "timer" $
    const "The function is broken" <$> ident "Check" <|>
    const "Use timer.Remove instead" <$> ident "Destroy"

-- | Warnings for the umsg library
umsgWarnings :: AParser String
umsgWarnings = libraryWarnings "umsg" $
    const "Use net messages." <$> ident "Start"

-- | Warnings for the util library
utilWarnings :: AParser String
utilWarnings = libraryWarnings "util" $
    const "Use tobool, without the util bit" <$> ident "tobool" <|>
    const "The function is broken" <$> ident "TraceEntityHull"


-- | Warnings for meta functions
metaFuncWarnings :: AParser String
metaFuncWarnings = do
    pMTok Colon

    -- CLuaLocomotion functions
    const  "Use :IsUsingLadder instead"                                     <$> ident "IsAscendingOrDescendingLadder"       <|>

     -- Panel functions
     const "Use :GetPaintBackground instead"                                <$> ident "GetDrawBackground"                   <|>
     const "Use :SetPaintBackground instead"                                <$> ident "SetDrawBackground"                   <|>
     const "The function is broken"                                         <$> ident "AddText"                             <|>
     const "Only used by deprecated Derma controls"                         <$> ident "PostMessage"                         <|>
     const "Only used in deprecated Derma controls"                         <$> ident "SetActionFunction"                   <|>
     const "Use :SetKeyboardInputEnabled instead"                           <$> ident "SetKeyBoardInputEnabled"             <|>
     const "The function is broken"                                         <$> ident "SetPaintFunction"                    <|>
     const "Use :SetTooltip instead, notice the lowercase fucking t"        <$> ident "SetToolTip"                          <|>
     const "use :SetTooltipPanel instead, notice the lowercase fucking t"   <$> ident "SetToolTipPanel"                     <|>
     const "Use :IsValid instead"                                           <$> ident "Valid"                               <|>

     -- Entity functions
     const "Use :GetHitBoxBone instead, note the capital fucking B"         <$> ident "GetHitboxBone"                       <|>
     const "Use :GetNWAngle instead"                                        <$> ident "GetNetworkedAngle"                   <|>
     const "Use :GetNWBool instead"                                         <$> ident "GetNetworkedBool"                    <|>
     const "Use :GetNWEntity instead"                                       <$> ident "GetNetworkedEntity"                  <|>
     const "Use :GetNWFloat instead"                                        <$> ident "GetNetworkedFloat"                   <|>
     const "Use :GetNWInt instead"                                          <$> ident "GetNetworkedInt"                     <|>
     const "Use :GetNWString instead"                                       <$> ident "GetNetworkedString"                  <|>
     const "Use :GetNWVarProxy instead"                                     <$> ident "GetNetworkedVarProxy"                <|>
     const "Use :GetNWVarTable instead"                                     <$> ident "GetNetworkedVarTable"                <|>
     const "Use :GetNWVector instead"                                       <$> ident "GetNetworkedVector"                  <|>
     const "The function is broken"                                         <$> ident "GetWorkshopID"                       <|>
     const "Use :SetParent instead"                                         <$> ident "SetAttachment"                       <|>
     const "Use :SetNWAngle instead"                                        <$> ident "SetNetworkedAngle"                   <|>
     const "Use :SetNWBool instead"                                         <$> ident "SetNetworkedBool"                    <|>
     const "Use :SetNWEntity instead"                                       <$> ident "SetNetworkedEntity"                  <|>
     const "Use :SetNWFloat instead"                                        <$> ident "SetNetworkedFloat"                   <|>
     const "Use :SetNWInt instead"                                          <$> ident "SetNetworkedInt"                     <|>
     const "Use :SetNWString instead"                                       <$> ident "SetNetworkedString"                  <|>
     const "Use :SetNWVarProxy instead"                                     <$> ident "SetNetworkedVarProxy"                <|>
     const "Use :SetNWVector instead"                                       <$> ident "SetNetworkedVector"                  <|>

     -- Player functions
     const "Use :GetViewPunchAngles instead"                                <$> ident "GetPunchAngle"                       <|>

     -- Material functions
     const "The function is broken"                                         <$> ident "SetShader"                           <|>

     -- Vector functions
     const "Use :Dot instead"                                               <$> ident "DotProduct"


-- | Parser for all deprecated sequences of tokens
deprecatedSequence :: LintSettings -> AParser String
deprecatedSequence opts = if not (lint_deprecated opts) then parserZero else (++) "Deprecated: " <$> (

    -- Deprecated meta functions
    try metaFuncWarnings                                                                                        <|>

    -- Library functions
    try aiWarnings                                                                                              <|>
    try mathWarnings                                                                                            <|>
    try spawnmenuWarnings                                                                                       <|>
    try stringWarnings                                                                                          <|>
    try surfaceWarnings                                                                                         <|>
    try tableWarnings                                                                                           <|>
    try timerWarnings                                                                                           <|>
    try umsgWarnings                                                                                            <|>
    try utilWarnings                                                                                            <|>

    -- Global functions
    const "Use collectgarbage(\"count\") instead"                   <$> ident "gcinfo"                          <|>
    const "Use ConVar objects instead"                              <$> ident "GetConVarNumber"                 <|>
    const "Use ConVar objects instead"                              <$> ident "GetConVarString"                 <|>
    const "Use AddCSLuaFile in the file itself instead"             <$> ident "IncludeCS"                       <|>
    const "Use ScreenScale instead"                                 <$> ident "SScale"                          <|>
    const "Use IsUselessModel instead"                              <$> ident "UTIL_IsUselessModel"             <|>
    const "Use IsValid instead"                                     <$> ident "ValidPanel"                      <|>
    const "Use net messages."                                       <$> ident "SendUserMessage"                  )

-- | Parser for all beginner mistakes
beginnerMistakeSequence :: LintSettings -> AParser String
beginnerMistakeSequence opts = if not (lint_beginnerMistakes opts) then parserZero else
    try (const "There's little fucking reason to use ';' in the first place, don't use it twice in a row" <$> pMTok Semicolon <* pMTok Semicolon) <|>
    try (const "The server already knows who sent the net message, use the first parameter of net.Receive" <$> do
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
    try (const "Jesus christ fuck off already" <$> do
        pMTok While
        whitespace
        pMTok TTrue
        whitespace
        pMTok Do
        whitespace
        pMTok End
        )

whiteSpaceStyleSequence :: LintSettings -> AParser String
whiteSpaceStyleSequence opts = if not (lint_whitespaceStyle opts) then parserZero else (++) "Style: " <$> (
    try (const "Please put some whitespace after 'if'" <$> pMTok If <* notFollowedBy whitespace)            <|>
    try (const "Please put some whitespace after 'elseif'" <$> pMTok Elseif <* notFollowedBy whitespace)    <|>
    try (const "Please put some whitespace after 'while'" <$> pMTok While <* notFollowedBy whitespace)      <|>
    try (const "Please put some whitespace after 'until'" <$> pMTok Until <* notFollowedBy whitespace)      <|>

    try (const "Please put some whitespace after ')'" <$> do
        pMTok RRound
        pTSatisfy (\t -> not (isWhitespace t) && t `notElem` [Colon, RRound, LRound, LSquare, RSquare, LCurly, RCurly, Comma, Dot, Semicolon])
        ) <|>
    try (const "Please put some whitespace before the operator" <$> do
        notWhitespace
        choice [pMTok Plus, pMTok Multiply, pMTok Divide, pMTok Modulus, pMTok TEq, pMTok TNEq, pMTok TCNEq, pMTok TLEQ, pMTok TGEQ, pMTok TLT, pMTok TGT, pMTok Equals, pMTok Concatenate, pMTok And, pMTok CAnd, pMTok Or, pMTok COr]
        ) <|>
    try (const "Please put some whitespace after the operator" <$> do
        choice [pMTok Plus, pMTok Multiply, pMTok Divide, pMTok Modulus, pMTok TEq, pMTok TNEq, pMTok TCNEq, pMTok TLEQ, pMTok TGEQ, pMTok TLT, pMTok TGT, pMTok Equals, pMTok Concatenate, pMTok And, pMTok CAnd, pMTok Or, pMTok COr]
        notWhitespace
        )
    )

-- | Parser for all profanity
profanitySequence :: LintSettings -> AParser String
profanitySequence opts = if not (lint_profanity opts) then parserZero else const "Watch your profanity" <$> (
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
badSequence :: LintSettings -> AParser String
badSequence opts = deprecatedSequence opts          <|>
                    profanitySequence opts          <|>
                    beginnerMistakeSequence opts    <|>
                    whiteSpaceStyleSequence opts

-- | Creates a warning for a certain sequence at any position
badSequenceWarning :: LineColPos -> String -> [String] -> [String]
badSequenceWarning pos wrn = (:) (showString "[Warning] " . showString (renderPos pos) . showString ": " . showString wrn $ "")

-- | Searches for all the bad sequences
badSequenceParser :: LintSettings -> AParser [String]
badSequenceParser opts =
    -- A bad sequence
    badSequenceWarning <$> pPos <*> badSequence opts <*> badSequenceParser opts <|>
    -- Continue searching
    anyToken *> badSequenceParser opts <|>
    -- end of input
    return []

-- | Returns all the warnings for a lexicon
sequenceWarnings :: LintSettings -> [MToken] -> [String]
sequenceWarnings opts mts = case execAParser "source.lua" (badSequenceParser opts) mts of
    Left _ -> error "[Error] line 1, column 1: Sequence finding error! Report an issue!"
    Right warnings -> warnings

-- | Helper function: check from string
checkFromString :: AParser a -> String -> Either ParseError a
checkFromString p inp = do
    lexed <- PSL.execParseTokens inp
    execAParser "source.lua" p lexed
