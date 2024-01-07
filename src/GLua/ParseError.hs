-- | This is a copy-paste-edit of Parsec's parse error rendering.
module GLua.ParseError (renderParseError) where

import Data.List (intercalate, nub)
import Text.Parsec.Error (
  Message (..),
  ParseError,
  errorMessages,
  messageString,
 )

-- | Render a parsec error
renderParseError :: ParseError -> String
renderParseError = showErrorMessages . errorMessages

-- | Modified version of the following function:
-- https://hackage.haskell.org/package/parsec/docs/src/Text.Parsec.Error.html#showErrorMessages.
--
-- At some point it would be nice to switch to another parser library (e.g. megaparsec), which has
-- more structured error types, and a nicer error rendering algorithm. Until then, this modified
-- function will do.
showErrorMessages :: [Message] -> String
showErrorMessages msgs
  | null msgs = "unknown parse error"
  | otherwise = intercalate ", " $ clean [showSysUnExpect, showUnExpect, showExpect, showMessages]
  where
    (sysUnExpect, msgs1) = span (SysUnExpect "" ==) msgs
    (unExpect, msgs2) = span (UnExpect "" ==) msgs1
    (expect, messages) = span (Expect "" ==) msgs2

    showExpect = showMany "expecting" expect
    showUnExpect = showMany "unexpected" unExpect
    showSysUnExpect
      | not (null unExpect) = ""
      | [] <- sysUnExpect = ""
      | msg : _ <- sysUnExpect, null (messageString msg) = "unexpected end of input"
      | msg : _ <- sysUnExpect = "unexpected " ++ messageString msg

    showMessages = showMany "" messages

    -- helpers
    showMany pre msgs3 = case clean (map messageString msgs3) of
      [] -> ""
      ms
        | null pre -> commasOr ms
        | otherwise -> pre ++ " " ++ commasOr ms

    commasOr [] = ""
    commasOr [m] = m
    commasOr ms = commaSep (init ms) ++ " or " ++ last ms

    commaSep = separate ", " . clean

    separate _ [] = ""
    separate _ [m] = m
    separate sep (m : ms) = m ++ sep ++ separate sep ms

    clean = nub . filter (not . null)
