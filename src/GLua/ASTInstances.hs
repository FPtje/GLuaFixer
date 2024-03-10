{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Contains class instances and functions related to AST These are not put
-- in the AST.ag file as importing an AG file would copy the instance
-- declarations, causing duplicate instances errors.
module GLua.ASTInstances where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import GLua.AG.AST
import GLua.TokenTypes (encodingOptions)

instance ToJSON AST where
  toEncoding = Aeson.genericToEncoding encodingOptions

instance FromJSON AST where
  parseJSON = Aeson.genericParseJSON encodingOptions

instance ToJSON Block where
  toEncoding = Aeson.genericToEncoding encodingOptions

instance FromJSON Block where
  parseJSON = Aeson.genericParseJSON encodingOptions

instance ToJSON MStat where
  toEncoding = Aeson.genericToEncoding encodingOptions

instance FromJSON MStat where
  parseJSON = Aeson.genericParseJSON encodingOptions

instance ToJSON MElse where
  toEncoding = Aeson.genericToEncoding encodingOptions

instance FromJSON MElse where
  parseJSON = Aeson.genericParseJSON encodingOptions

instance ToJSON MElseIf where
  toEncoding = Aeson.genericToEncoding encodingOptions

instance FromJSON MElseIf where
  parseJSON = Aeson.genericParseJSON encodingOptions

instance ToJSON Stat where
  toEncoding = Aeson.genericToEncoding encodingOptions

instance FromJSON Stat where
  parseJSON = Aeson.genericParseJSON encodingOptions

instance ToJSON AReturn where
  toEncoding = Aeson.genericToEncoding encodingOptions

instance FromJSON AReturn where
  parseJSON = Aeson.genericParseJSON encodingOptions

instance ToJSON FuncName where
  toEncoding = Aeson.genericToEncoding encodingOptions

instance FromJSON FuncName where
  parseJSON = Aeson.genericParseJSON encodingOptions

instance ToJSON PrefixExp where
  toEncoding = Aeson.genericToEncoding encodingOptions

instance FromJSON PrefixExp where
  parseJSON = Aeson.genericParseJSON encodingOptions

instance ToJSON PFExprSuffix where
  toEncoding = Aeson.genericToEncoding encodingOptions

instance FromJSON PFExprSuffix where
  parseJSON = Aeson.genericParseJSON encodingOptions

instance ToJSON MExpr where
  toEncoding = Aeson.genericToEncoding encodingOptions

instance FromJSON MExpr where
  parseJSON = Aeson.genericParseJSON encodingOptions

instance ToJSON Expr where
  toEncoding = Aeson.genericToEncoding encodingOptions

instance FromJSON Expr where
  parseJSON = Aeson.genericParseJSON encodingOptions

instance ToJSON Args where
  toEncoding = Aeson.genericToEncoding encodingOptions

instance FromJSON Args where
  parseJSON = Aeson.genericParseJSON encodingOptions

instance ToJSON Field where
  toEncoding = Aeson.genericToEncoding encodingOptions

instance FromJSON Field where
  parseJSON = Aeson.genericParseJSON encodingOptions

instance ToJSON FieldSep where
  toEncoding = Aeson.genericToEncoding encodingOptions

instance FromJSON FieldSep where
  parseJSON = Aeson.genericParseJSON encodingOptions

instance ToJSON BinOp where
  toEncoding = Aeson.genericToEncoding encodingOptions

instance FromJSON BinOp where
  parseJSON = Aeson.genericParseJSON encodingOptions

instance ToJSON UnOp where
  toEncoding = Aeson.genericToEncoding encodingOptions

instance FromJSON UnOp where
  parseJSON = Aeson.genericParseJSON encodingOptions
