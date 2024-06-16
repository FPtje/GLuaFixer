{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Contains class instances and functions related to AST These are not put
-- in the AST.ag file as importing an AG file would copy the instance
-- declarations, causing duplicate instances errors.
module GLua.ASTInstances where

import GLua.AG.AST
import GLua.EncodingOptions (encodingOptions)

import Data.Aeson.TH (deriveJSON)

$( concat
    <$> mapM
      (deriveJSON encodingOptions)
      [ ''AST
      , ''Block
      , ''MStat
      , ''MElse
      , ''MElseIf
      , ''Stat
      , ''AReturn
      , ''FuncName
      , ''PrefixExp
      , ''PFExprSuffix
      , ''MExpr
      , ''Expr
      , ''Args
      , ''Field
      , ''FieldSep
      , ''BinOp
      , ''UnOp
      ]
 )
