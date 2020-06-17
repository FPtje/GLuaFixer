{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Contains class instances and functions related to AST These are not put
-- in the AST.ag file as importing an AG file would copy the instance
-- declarations, causing duplicate instances errors.
module GLua.ASTInstances where

import GLua.AG.AST
import Data.Aeson

instance ToJSON AST where
instance FromJSON AST where

instance ToJSON Block where
instance FromJSON Block where

instance ToJSON MStat where
instance FromJSON MStat where

instance ToJSON MElse where
instance FromJSON MElse where

instance ToJSON MElseIf where
instance FromJSON MElseIf where

instance ToJSON Stat where
instance FromJSON Stat where

instance ToJSON AReturn where
instance FromJSON AReturn where

instance ToJSON FuncName where
instance FromJSON FuncName where

instance ToJSON PrefixExp where
instance FromJSON PrefixExp where

instance ToJSON PFExprSuffix where
instance FromJSON PFExprSuffix where

instance ToJSON MExpr where
instance FromJSON MExpr where

instance ToJSON Expr where
instance FromJSON Expr where

instance ToJSON Args where
instance FromJSON Args where

instance ToJSON Field where
instance FromJSON Field where

instance ToJSON FieldSep where
instance FromJSON FieldSep where

instance ToJSON BinOp where
instance FromJSON BinOp where

instance ToJSON UnOp where
instance FromJSON UnOp where
