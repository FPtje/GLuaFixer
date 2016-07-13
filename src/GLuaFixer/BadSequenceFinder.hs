{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module GLuaFixer.BadSequenceFinder (sequenceWarnings) where


import GLua.AG.Token
import qualified GLua.TokenTypes as TT
--import GLua.PSParser
import GLuaFixer.LintMessage
import GLuaFixer.LintSettings
--import Text.Megaparsec hiding (Token)
--import Data.Attoparsec
import Data.Attoparsec.Types
import qualified Data.Attoparsec.Internal as I
import qualified Data.Attoparsec.Internal.Types as T
import Data.Attoparsec.Combinator

type AParser = T.Parser [[MToken]]
type instance T.State [[MToken]] = [MToken]

type RegionalMessage = (String, Region)

instance Chunk [[MToken]] where
  type ChunkElem [[MToken]] = MToken

  nullChunk [chunk] = null chunk
  pappendChunk toks [ts] = ts ++ toks
  atBufferEnd _ toks = T.Pos (length toks)
  bufferElemAt _ (T.Pos pos) ts =
    if length ts > pos then
      Just (ts !! pos, 1)
    else
      Nothing
  chunkElemToChar _ _ = 'a'--nus

pMSatisfy :: (MToken -> Bool) -> AParser MToken
pMSatisfy f =
  do
    tok : _ <- peekToken
    if f tok then
      advance 1 >> return tok
    else
      fail "satisfy"

advance :: Int -> AParser ()
advance n = T.Parser $ \t pos more _lose succc ->
  succc t (pos + T.Pos n) more ()
{-# INLINE advance #-}

substring :: T.Pos -> T.Pos -> [MToken] -> [[MToken]]
substring (T.Pos pos) (T.Pos n) mtoks = [take n $ drop pos mtoks]

lengthAtLeast :: T.Pos -> Int -> [MToken] -> Bool
lengthAtLeast (T.Pos pos) n bs = length bs >= pos + n

ensureSuspended :: Int -> [MToken] -> T.Pos -> T.More
                -> T.Failure [[MToken]] [MToken] r -- Failure r
                -> T.Success [[MToken]] [MToken] [[MToken]] r -- Success MToken r
                -> IResult [[MToken]] r -- Result r
ensureSuspended n t pos more lose succc =
    T.runParser (I.demandInput >> go) t pos more lose succc
  where go = T.Parser $ \t' pos' more' lose' succ' ->
          if lengthAtLeast pos' n t'
          then succ' t' pos' more' (substring pos (T.Pos n) t')
          else T.runParser (I.demandInput >> go) t' pos' more' lose' succ'

-- | Match any token, to perform lookahead.  Does not consume any
-- input, but will fail if end of input has been reached.
peekToken :: AParser [MToken]
peekToken = T.Parser $ \t pos@(T.Pos p) more lose succc ->
    if lengthAtLeast pos 1 t
    then succc t pos more (drop p t)
    else let succ' t' pos' more' bs' = succc t' pos' more' $! head bs'
         in ensureSuspended 1 t pos more lose succ'

-- | Satisfy for normal tokens
pTSatisfy :: (Token -> Bool) -> AParser MToken
pTSatisfy f = pMSatisfy f'
 where
    f' :: MToken -> Bool
    f' (MToken _ t) = f t

pMTok :: Token -> AParser MToken
pMTok t = pTSatisfy ((==) t)

-- | A hint message with a region attached to it
regionalMessage :: String -> MToken -> RegionalMessage
regionalMessage txt tk = (txt, TT.mpos tk)

-- | Prepend a text to a regional message
prependMessage :: String -> RegionalMessage -> RegionalMessage
prependMessage s (msg, rg) = (s ++ msg, rg)


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
libraryWarnings :: String -> AParser RegionalMessage -> AParser RegionalMessage
libraryWarnings s p = do
    MToken rg _ <- ident s
    pMTok Dot

    (msg, rg') <- p
    return (msg, Region (TT.rgStart rg) (TT.rgEnd rg'))

-- | Warnings for the ai library
aiWarnings :: AParser RegionalMessage
aiWarnings = libraryWarnings "ai" $ choice
    [ regionalMessage "The function is broken" <$> ident "GetScheduleID"
    , regionalMessage "The function is broken" <$> ident "GetTaskID"]

-- | Warnings for the math library
mathWarnings :: AParser RegionalMessage
mathWarnings = libraryWarnings "math" $ choice
    [ regionalMessage "Use math.Distance instead" <$> ident "Dist"
    , regionalMessage "Use math.fmod instead" <$> ident "mod"]


-- | Warnings for the spawnmenu library
spawnmenuWarnings :: AParser RegionalMessage
spawnmenuWarnings = libraryWarnings "spawnmenu" $ choice
    [ regionalMessage "Use spawnmenu.SaveToTextFiles instead" <$> ident "DoSaveToTextFiles"
    , regionalMessage "Use spawnmenu.PopulateFromTextFiles instead" <$> ident "PopulateFromEngineTextFiles"
    , regionalMessage "The function is broken" <$> ident "SwitchToolTab"]

-- | Warnings for the string library
stringWarnings :: AParser RegionalMessage
stringWarnings = libraryWarnings "string" $ choice
    [ regionalMessage "Use either string.sub(str, index, index) or str[index]" <$> ident "GetChar"
    , regionalMessage "Use string.gmatch instead" <$> ident "gfind"]

-- | Warnings for the surface library
surfaceWarnings :: AParser RegionalMessage
surfaceWarnings = libraryWarnings "libraryWarnings" $ choice
    [ regionalMessage "Use ScrH instead" <$> ident "ScreenHeight"
    , regionalMessage "Use ScrW instead" <$> ident "ScreenWidth"]

-- | Warnings for the table library
tableWarnings :: AParser RegionalMessage
tableWarnings = libraryWarnings "table" $ choice
    [regionalMessage "Use ipairs or something instead" <$> choice
        [ ident "FindNext"
        , ident "FindPrev"
        , ident "foreach"
        , ident "ForEach"
        , ident "foreachi"]
    , regionalMessage "Use next instead" <$> choice
        [ ident "GetFirstKey"
        , ident "GetFirstValue"]
    , regionalMessage "Use #tbl instead"      <$> ident "GetLastKey"
    , regionalMessage "Use tbl[#tbl] instead" <$> ident "GetLastValue"
    , regionalMessage "Use #tbl instead"      <$> ident "getn"]


-- | Warnings for the timer library
timerWarnings :: AParser RegionalMessage
timerWarnings = libraryWarnings "timer" $ choice
    [ regionalMessage "The function is broken" <$> ident "Check"
    , regionalMessage "Use timer.Remove instead" <$> ident "Destroy"]

-- | Warnings for the umsg library
umsgWarnings :: AParser RegionalMessage
umsgWarnings = libraryWarnings "umsg" $
    regionalMessage "Use net messages." <$> ident "Start"

-- | Warnings for the util library
utilWarnings :: AParser RegionalMessage
utilWarnings = libraryWarnings "util" $ choice
    [ regionalMessage "Use tobool, without the util bit" <$> ident "tobool"
    , regionalMessage "The function is broken" <$> ident "TraceEntityHull"]


-- | Warnings for meta functions
metaFuncWarnings :: AParser RegionalMessage
metaFuncWarnings = do
    pMTok Colon

    -- CLuaLocomotion functions
    choice [ regionalMessage  "Use :IsUsingLadder instead"                                     <$> ident "IsAscendingOrDescendingLadder"

     -- Panel functions
     , regionalMessage "Use :GetPaintBackground instead"                                <$> ident "GetDrawBackground"
     , regionalMessage "Use :SetPaintBackground instead"                                <$> ident "SetDrawBackground"
     , regionalMessage "The function is broken"                                         <$> ident "AddText"
     , regionalMessage "Only used by deprecated Derma controls"                         <$> ident "PostMessage"
     , regionalMessage "Only used in deprecated Derma controls"                         <$> ident "SetActionFunction"
     , regionalMessage "Use :SetKeyboardInputEnabled instead"                           <$> ident "SetKeyBoardInputEnabled"
     , regionalMessage "The function is broken"                                         <$> ident "SetPaintFunction"
     , regionalMessage "Use :SetTooltip instead, notice the lowercase fucking t"        <$> ident "SetToolTip"
     , regionalMessage "use :SetTooltipPanel instead, notice the lowercase fucking t"   <$> ident "SetToolTipPanel"
     , regionalMessage "Use :IsValid instead"                                           <$> ident "Valid"

     -- Entity functions
     , regionalMessage "Use :GetHitBoxBone instead, note the capital fucking B"         <$> ident "GetHitboxBone"
     , regionalMessage "Use :GetNWAngle instead"                                        <$> ident "GetNetworkedAngle"
     , regionalMessage "Use :GetNWBool instead"                                         <$> ident "GetNetworkedBool"
     , regionalMessage "Use :GetNWEntity instead"                                       <$> ident "GetNetworkedEntity"
     , regionalMessage "Use :GetNWFloat instead"                                        <$> ident "GetNetworkedFloat"
     , regionalMessage "Use :GetNWInt instead"                                          <$> ident "GetNetworkedInt"
     , regionalMessage "Use :GetNWString instead"                                       <$> ident "GetNetworkedString"
     , regionalMessage "Use :GetNWVarProxy instead"                                     <$> ident "GetNetworkedVarProxy"
     , regionalMessage "Use :GetNWVarTable instead"                                     <$> ident "GetNetworkedVarTable"
     , regionalMessage "Use :GetNWVector instead"                                       <$> ident "GetNetworkedVector"
     , regionalMessage "The function is broken"                                         <$> ident "GetWorkshopID"
     --const "Use :SetParent instead"                                         <$> ident "SetAttachment"
     , regionalMessage "Use :SetNWAngle instead"                                        <$> ident "SetNetworkedAngle"
     , regionalMessage "Use :SetNWBool instead"                                         <$> ident "SetNetworkedBool"
     , regionalMessage "Use :SetNWEntity instead"                                       <$> ident "SetNetworkedEntity"
     , regionalMessage "Use :SetNWFloat instead"                                        <$> ident "SetNetworkedFloat"
     , regionalMessage "Use :SetNWInt instead"                                          <$> ident "SetNetworkedInt"
     , regionalMessage "Use :SetNWString instead"                                       <$> ident "SetNetworkedString"
     , regionalMessage "Use :SetNWVarProxy instead"                                     <$> ident "SetNetworkedVarProxy"
     , regionalMessage "Use :SetNWVector instead"                                       <$> ident "SetNetworkedVector"

     -- Player functions
     , regionalMessage "Use :GetViewPunchAngles instead"                                <$> ident "GetPunchAngle"

     -- Material functions
     , regionalMessage "The function is broken"                                         <$> ident "SetShader"

     -- Vector functions
     , regionalMessage "Use :Dot instead"                                               <$> ident "DotProduct"]


-- | Parser for all deprecated sequences of tokens
deprecatedSequence :: LintSettings -> AParser RegionalMessage
deprecatedSequence opts = if not (lint_deprecated opts) then fail "deprecated" else prependMessage "Deprecated: " <$> choice

    -- Deprecated meta functions
    [ try metaFuncWarnings

    -- Library functions
    , try aiWarnings
    , try mathWarnings
    , try spawnmenuWarnings
    , try stringWarnings
    , try surfaceWarnings
    , try tableWarnings
    , try timerWarnings
    , try umsgWarnings
    , try utilWarnings

    -- Global functions
    , regionalMessage "Use collectgarbage(\"count\") instead"                   <$> ident "gcinfo"
    , regionalMessage "Use ConVar objects instead"                              <$> ident "GetConVarNumber"
    , regionalMessage "Use ConVar objects instead"                              <$> ident "GetConVarString"
    , regionalMessage "Use AddCSLuaFile in the file itself instead"             <$> ident "IncludeCS"
    , regionalMessage "Use ScreenScale instead"                                 <$> ident "SScale"
    , regionalMessage "Use IsUselessModel instead"                              <$> ident "UTIL_IsUselessModel"
    , regionalMessage "Use IsValid instead"                                     <$> ident "ValidPanel"
    , regionalMessage "Use net messages."                                       <$> ident "SendUserMessage"]

-- | Parser for all beginner mistakes
beginnerMistakeSequence :: LintSettings -> AParser RegionalMessage
beginnerMistakeSequence opts = if not (lint_beginnerMistakes opts) then fail "beginner" else choice
    [ try (regionalMessage "There's little fucking reason to use ';' in the first place, don't use it twice in a row" <$> pMTok Semicolon <* pMTok Semicolon)
    , try (regionalMessage "The server already knows who sent the net message, use the first parameter of net.Receive" <$> do
        ident "net"
        pMTok Dot
        ident "WriteEntity"
        pMTok LRound
        option undefined whitespace
        ident "LocalPlayer"
        option undefined whitespace
        pMTok LRound
        option undefined whitespace
        pMTok RRound
        )
    , try (regionalMessage "Jesus christ fuck off already" <$> do
        pMTok While
        whitespace
        pMTok TTrue
        whitespace
        pMTok Do
        whitespace
        pMTok End
        )
    ]

whiteSpaceStyleSequence :: LintSettings -> AParser RegionalMessage
whiteSpaceStyleSequence opts = if not (lint_whitespaceStyle opts) then fail "style" else prependMessage "Style: " <$> choice
    [ try (regionalMessage "Please put some whitespace after 'if'" <$> pMTok If <* notWhitespace)
    , try (regionalMessage "Please put some whitespace after 'elseif'" <$> pMTok Elseif <* notWhitespace)
    , try (regionalMessage "Please put some whitespace after 'while'" <$> pMTok While <* notWhitespace)
    , try (regionalMessage "Please put some whitespace after 'until'" <$> pMTok Until <* notWhitespace)

    , try (regionalMessage "Please put some whitespace after ')'" <$> do
        pMTok RRound
        pTSatisfy (\t -> not (isWhitespace t) && t `notElem` [Colon, RRound, LRound, LSquare, RSquare, LCurly, RCurly, Comma, Dot, Semicolon])
        )
    , try (regionalMessage "Please put some whitespace before the operator" <$> do
        notWhitespace
        choice [pMTok Plus, pMTok Multiply, pMTok Divide, pMTok Modulus, pMTok TEq, pMTok TNEq, pMTok TCNEq, pMTok TLEQ, pMTok TGEQ, pMTok TLT, pMTok TGT, pMTok Equals, pMTok Concatenate, pMTok And, pMTok CAnd, pMTok Or, pMTok COr]
        )
    , try (regionalMessage "Please put some whitespace after the operator" <$> do
        choice [pMTok Plus, pMTok Multiply, pMTok Divide, pMTok Modulus, pMTok TEq, pMTok TNEq, pMTok TCNEq, pMTok TLEQ, pMTok TGEQ, pMTok TLT, pMTok TGT, pMTok Equals, pMTok Concatenate, pMTok And, pMTok CAnd, pMTok Or, pMTok COr]
        notWhitespace
        )
    ]

-- | Simple failure
--parserZero :: AParser a
--parserZero = failure Set.empty Set.empty Set.empty

-- | Parser for all profanity
profanitySequence :: LintSettings -> AParser RegionalMessage
profanitySequence opts = if not (lint_profanity opts) then fail "no profanity" else regionalMessage "Watch your profanity" <$> choice
    [ ident "anus"
    , ident "bitch"
    , ident "cock"
    , ident "cocks"
    , ident "cunt"
    , ident "dick"
    , ident "dicks"
    , ident "fuck"
    , ident "fucking"
    , ident "goddamnit"
    , ident "knob"
    , ident "knobs"
    , ident "motherfucker"
    , ident "nigger"
    , ident "niggers"
    , ident "niggertits"
    , ident "nipple"
    , ident "shit"
    ]

-- | Parses for any bad sequence
badSequence :: LintSettings -> AParser RegionalMessage
badSequence opts = choice
  [ deprecatedSequence opts
  , profanitySequence opts
  , beginnerMistakeSequence opts
  , whiteSpaceStyleSequence opts]

-- | Add a warning for a bad sequence
badSequenceWarning :: RegionalMessage -> [String -> LintMessage] -> [String -> LintMessage]
badSequenceWarning (msg, rg) = (:) (LintWarning rg msg)

-- | Searches for all the bad sequences
badSequenceParser :: LintSettings -> AParser [String -> LintMessage]
badSequenceParser opts = choice
    -- A bad sequence
    [ badSequenceWarning <$> badSequence opts <*> badSequenceParser opts
    -- Continue searching
    , advance 1 *> badSequenceParser opts
    -- end of input
    , return []
    ]


successK :: T.Success [[MToken]] [MToken] r r
successK t (T.Pos pos) _more = T.Done [drop pos t]

failK :: T.Failure [[MToken]] [MToken] r
failK t (T.Pos pos) _more = T.Fail [drop pos t]

-- | Returns all the warnings for a lexicon
sequenceWarnings :: LintSettings -> [MToken] -> [String -> LintMessage]
sequenceWarnings opts mts =
  --case T.runParser (badSequenceParser opts) mts (T.Pos 0) T.Complete (\t (T.Pos pos) _ stack msg -> T.Fail _ stack msg) (\t (T.Pos pos) _ a -> T.Done _ a) of
  case T.runParser (badSequenceParser opts) mts (T.Pos 0) T.Complete failK successK of
    T.Fail _ _ _ -> error "[Error] line 1, column 1: Sequence finding error! Report an issue!"
    T.Done _ res -> res
    --case execAParser "source.lua" (badSequenceParser opts) mts of
    --Left _ -> error "[Error] line 1, column 1: Sequence finding error! Report an issue!"
    --Right warnings -> warnings

