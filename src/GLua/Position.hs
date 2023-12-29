{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reimplements the LineColPos from `uu-parsinglib`, and adds some more functions related to
-- positioning.
module GLua.Position where

import Data.Aeson
import GHC.Generics (Generic)

data LineColPos = LineColPos {lcpLine :: !Int, lcpColumn :: !Int, lcpPos :: !Int}
  deriving (Eq, Show)

-- The order depends on line and column, not the position, though this should not make a difference
-- when both LineColPoses are from the same file.
instance Ord LineColPos where
  compare (LineColPos l c _) (LineColPos l' c' _) =
    compare l l' `mappend` compare c c'

instance ToJSON LineColPos where
  -- this generates a Value
  toJSON (LineColPos line col p) =
    object ["line" .= line, "column" .= col, "pos" .= p]

#if MIN_VERSION_aeson(0,10,0)
  -- this encodes directly to a bytestring Builder
  toEncoding (LineColPos line col p) =
    pairs ("line" .= line <> "column" .= col <> "pos" .= p)
#endif

instance FromJSON LineColPos where
  parseJSON = withObject "LineColPos" $ \v ->
    LineColPos
      <$> v .: "line"
      <*> v .: "column"
      <*> v .: "pos"

data Region = Region {rgStart :: !LineColPos, rgEnd :: !LineColPos}
  deriving (Eq, Show, Generic)

-- Ord instance defined explicitly for clarity.
instance Ord Region where
  compare (Region s e) (Region s' e') =
    compare s s' `mappend` compare e e'

instance ToJSON Region
instance FromJSON Region

-- | An empty region from position 0 to position 0.
emptyRg :: Region
emptyRg = Region (LineColPos 0 0 0) (LineColPos 0 0 0)

-- | Hack: Chooses left region if it is not 'emptyRg', and the right region if it is.
rgOr :: Region -> Region -> Region
rgOr l r
  | l == emptyRg = r
  | otherwise = l
