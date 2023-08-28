{-# LANGUAGE DerivingStrategies #-}

module Templatespiler.Convert.Warning where

import Prettyprinter (Doc, Pretty (pretty), (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)
import Templatespiler.Convert.Targets (TargetLanguage)
import Templatespiler.IR.Imperative

data ImperativeConvertWarning
  = CantConvertType VarType TargetLanguage
  | CantConvertExpr Expr TargetLanguage
  deriving stock (Show)

prettyVarType :: VarType -> Doc AnsiStyle
prettyVarType vt = case vt of
  StringType -> "String"
  IntType -> "Int"
  FloatType -> "Float"
  ArrayType len t -> "tuple or struct"
  UnknownType binding -> "Unknown type for binding" <+> show binding
