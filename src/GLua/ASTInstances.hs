{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Contains class instances and functions related to AST These are not put
-- in the AST.ag file as importing an AG file would copy the instance
-- declarations, causing duplicate instances errors.
module GLua.ASTInstances where

import Data.Aeson
import GLua.AG.AST

instance ToJSON AST
instance FromJSON AST

instance ToJSON Block
instance FromJSON Block

instance ToJSON MStat
instance FromJSON MStat

instance ToJSON MElse
instance FromJSON MElse

instance ToJSON MElseIf
instance FromJSON MElseIf

instance ToJSON Stat
instance FromJSON Stat

instance ToJSON AReturn
instance FromJSON AReturn

instance ToJSON FuncName
instance FromJSON FuncName

instance ToJSON PrefixExp
instance FromJSON PrefixExp

instance ToJSON PFExprSuffix
instance FromJSON PFExprSuffix

instance ToJSON MExpr
instance FromJSON MExpr

instance ToJSON Expr
instance FromJSON Expr

instance ToJSON Args
instance FromJSON Args

instance ToJSON Field
instance FromJSON Field

instance ToJSON FieldSep
instance FromJSON FieldSep

instance ToJSON BinOp
instance FromJSON BinOp

instance ToJSON UnOp
instance FromJSON UnOp
