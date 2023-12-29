-- | A parser that takes a maximum line length as input, and emits warnings for any line that
-- exceeds it.
module GLua.LineLimitParser where

import Control.Applicative ((<|>))
import Control.Monad (void)
import GLua.Lexer (pPos)
import GLua.Position (Region (..))
import GLuaFixer.LintMessage (Issue (LineTooLong), LintMessage (..), Severity (LintWarning))
import Text.Parsec (endOfLine, eof, parse, satisfy, skipMany1)
import Text.Parsec.String (Parser)

-- | The maximum line length
newtype LineLimit = LineLimit Int

execParseLineLimits :: FilePath -> LineLimit -> String -> [LintMessage]
execParseLineLimits filePath lineLimit@(LineLimit limit) contents =
  -- No point in parsing anything if the limit is 0
  if limit <= 0
    then []
    else case parse (lineLimitParser filePath lineLimit) "input" contents of
      Left parseError -> error $ "Parse error while checking line limit: " ++ show parseError
      Right lintWarnings -> lintWarnings

-- | Parser that produces warnings about lines being too long
lineLimitParser :: FilePath -> LineLimit -> Parser [LintMessage]
lineLimitParser filePath lineLimit =
  [] <$ eof
    <|> recurse filePath lineLimit

-- | Recursive case of 'lineLimitParser',
recurse :: FilePath -> LineLimit -> Parser [LintMessage]
recurse filePath lineLimit@(LineLimit limit) = do
  countAtMost limit notNewline
  -- If it finds line endings, it recurses without producing warnings. Otherwise, if it finds
  -- anything else, produce a warning and still recurse.
  endOfLines *> lineLimitParser filePath lineLimit
    <|> (:) <$> lineLimitWarning filePath <*> lineLimitParser filePath lineLimit

-- | Succeeds when a character is not a line
notNewline :: Parser Char
notNewline = satisfy $ \c -> c /= '\n' && c /= '\r'

-- | end of lines. Consumes all \r and \n characters it finds
endOfLines :: Parser ()
endOfLines = skipMany1 endOfLine <|> void eof

-- | Produces a line limit warning
lineLimitWarning :: FilePath -> Parser LintMessage
lineLimitWarning filePath = do
  startPos <- pPos
  skipMany1 notNewline
  endPos <- pPos

  let
    warnRegion = Region startPos endPos

  pure $ LintMessage LintWarning warnRegion LineTooLong filePath

-- | Consume the parser until either it has been parsed the Int amount of times, or the parser fails
countAtMost :: Int -> Parser a -> Parser ()
countAtMost 0 _ = pure ()
countAtMost i p = p *> countAtMost (i - 1) p <|> pure ()
