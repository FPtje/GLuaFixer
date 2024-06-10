{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reimplements the LineColPos from `uu-parsinglib`, and adds some more functions related to
-- positioning.
module GLua.Position where

import Data.Aeson
import Data.Foldable (toList)

data LineColPos = LineColPos {lcpLine :: !Int, lcpColumn :: !Int, lcpPos :: !Int}
  deriving (Eq, Show)

-- The order depends on line and column, not the position, though this should not make a difference
-- when both LineColPoses are from the same file.
instance Ord LineColPos where
  compare (LineColPos l c _) (LineColPos l' c' _) =
    compare l l' `mappend` compare c c'

instance ToJSON LineColPos where
  -- this generates a Value
  toJSON (LineColPos line col p) = toJSON [line, col, p]

#if MIN_VERSION_aeson(0,10,0)
  -- this encodes directly to a bytestring Builder
  toEncoding (LineColPos line col p) = toEncoding [line, col, p]
#endif

instance FromJSON LineColPos where
  parseJSON = withArray "LineColPos" $ \array ->
    case toList array of
      [line, col, pos] -> LineColPos <$> parseJSON line <*> parseJSON col <*> parseJSON pos
      _ -> fail "Expected tuple of line, column, position"

data Region = Region {rgStart :: !LineColPos, rgEnd :: !LineColPos}
  deriving (Eq, Show)

-- Ord instance defined explicitly for clarity.
instance Ord Region where
  compare (Region s e) (Region s' e') =
    compare s s' `mappend` compare e e'

instance ToJSON Region where
  toJSON (Region start end) = toJSON [toJSON start, toJSON end]

instance FromJSON Region where
  parseJSON = withArray "Region" $ \array ->
    case toList array of
      [start, end] -> Region <$> parseJSON start <*> parseJSON end
      _ -> fail "Expected tuple of [[line, column, position], [line, column, position]]"

-- | An empty region from position 0 to position 0.
emptyRg :: Region
emptyRg = Region (LineColPos 0 0 0) (LineColPos 0 0 0)

-- | Hack: Chooses left region if it is not 'emptyRg', and the right region if it is.
rgOr :: Region -> Region -> Region
rgOr l r
  | l == emptyRg = r
  | otherwise = l

-- | Whether the first region ends strictly before the second region starts
before :: Region -> Region -> Bool
before (Region _ (LineColPos _ _ p)) (Region (LineColPos _ _ p') _) = p < p'

-- | Whether the first region ends before or on the same line as the second region starts
beforeOrOnLine :: Region -> Region -> Bool
beforeOrOnLine (Region _ (LineColPos l _ _)) (Region (LineColPos l' _ _) _) = l <= l'

-- | Whether the first region ends before the second region ends
beforeEnd :: Region -> Region -> Bool
beforeEnd (Region _ (LineColPos _ _ p)) (Region _ (LineColPos _ _ p')) = p < p'

-- | Whether the first region ends before or on the same line as the END of the second region
beforeEndLine :: Region -> Region -> Bool
beforeEndLine (Region _ (LineColPos l _ _)) (Region _ (LineColPos l' _ _)) = l <= l'

-- | Returns a region that starts at the start of the first region
-- and ends BEFORE the start of the second region
upto :: Region -> Region -> Region
upto lr rr = case (rgEnd lr, rgStart rr) of
  (_, LineColPos 0 0 0) -> lr
  (LineColPos l c _, LineColPos l' c' _)
    | l' > l || (l' == l && c' > c) -> lr
    | otherwise -> Region (rgStart lr) (rgStart rr)
