{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A lightweight imperative language that is used as an IR
module Templatespiler.IR.Imperative where

import Templatespiler.IR.Common

import Prettyprinter

import Prelude hiding (group)

withSuffix :: VarName -> Text -> VarName
withSuffix (VarName (n :| ns)) suffix = VarName (n :| (ns <> [suffix]))

type Program = [Statement]

data Statement
  = -- | Variable declaration. This may be a C-style declaration (@int x@) or an assignment (@x = 0@) depending on the target language.
    Decl
      VarName
      -- ^ The variable name
      VarType
      -- ^ The type
  | Assign VarName VarType Expr
  | -- | Read 1 line of input, split it by some separator, and then read into separate variables
    MultiReadAssign
      Text
      -- ^ Separator
      (NonEmpty (VarName, ReadType))
  | For
      VarName -- variable name
      Expr -- start
      Expr -- end
      [Statement] -- body
  | AppendToArray
      VarName
      -- ^ array name
      Expr
      -- ^ index
      Expr
      -- ^ value
  deriving stock (Show)

data VarType
  = IntType
  | FloatType
  | StringType
  | ArrayType Expr VarType
  | DynamicArrayType VarType
  | TupleOrStructType (Maybe VarName) (NonEmpty VarType)
  | UnknownType
  deriving stock (Show)

data Expr
  = ConstInt Int
  | Var VarName
  | ReadAtom ReadType
  | TupleOrStruct (Maybe VarName) (NonEmpty Expr)
  deriving stock (Show)

data ReadType
  = ReadInt
  | ReadFloat
  | ReadString
  deriving stock (Show, Eq, Ord)

prettyExpr :: Expr -> Doc nn
prettyExpr (ConstInt i) = pretty i
prettyExpr (Var vn) = pretty vn
prettyExpr (ReadAtom rt) = "read" <+> pretty rt
prettyExpr (TupleOrStruct n es) = pretty n <> tupled (fmap prettyExpr (toList es))

prettyVarType :: VarType -> Doc ann
prettyVarType IntType = "Int"
prettyVarType FloatType = "Float"
prettyVarType StringType = "String"
prettyVarType (ArrayType e t) = prettyVarType t <> brackets (prettyExpr e)
prettyVarType (DynamicArrayType t) = prettyVarType t <> "*"
prettyVarType (TupleOrStructType n ts) = pretty n <> tupled (fmap prettyVarType (toList ts))
prettyVarType UnknownType = "UnknownType"

instance Pretty ReadType where
  pretty ReadInt = "@Int"
  pretty ReadFloat = "@Float"
  pretty ReadString = "@String"

instance Pretty Statement where
  pretty (Decl vn vt) = prettyVarType vt <+> pretty vn
  pretty (Assign vn vt e) = pretty vn <+> "=" <+> prettyExpr e
  pretty (MultiReadAssign sep vs) = "read" <+> dquotes (pretty sep) <+> hsep (fmap (\(vn, rt) -> pretty vn <> pretty rt) (toList vs))
  pretty (For vn start end body) =
    group $
      vsep
        [ nest 2 $
            vsep
              [ "for" <+> parens (pretty vn <+> "in" <+> prettyExpr start <> ".." <> prettyExpr end) <+> "{"
              , vsep $ toList $ fmap pretty body
              ]
        , "}"
        ]
  pretty (AppendToArray vn idx val) = pretty vn <> brackets (prettyExpr idx) <+> "=" <+> prettyExpr val

prettyProgram :: Program -> Doc ann
prettyProgram = vsep . fmap pretty
