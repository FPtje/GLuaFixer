-- | Json encoding options
module GLua.EncodingOptions where

import qualified Data.Aeson as Aeson

-- | Default glualint json encoding options
encodingOptions :: Aeson.Options
encodingOptions =
  Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    , Aeson.sumEncoding = Aeson.ObjectWithSingleField
    }
