{-# LANGUAGE DerivingStrategies #-}

{- | Imperative flavoured IR for templatespiler.
 While there's different details, all imperative languages are gonna largely have the same structure:
 - read how many inputs there are
 - loop over said variable and read more things
 - insert to an array if necessary
 - repeat if necessary
 - print something (not our concern)
-}
module Templatespiler.IR.Imperative where

import Prettyprinter
import Templatespiler.IR.Common
import Prelude hiding (Type, group)

data Terminal
  = StringTerminal
  | IntegerTerminal
  | FloatTerminal
  deriving stock (Show, Eq)

data Type
  = TerminalType Terminal
  | -- | An "array", which may be a list or a fixed size array depending on target language. The length must be known at compile time.
    ArrayType
      Expr
      Type
  | TupleOrStructType VarName (NonEmpty (VarName, Type))
  | DynamicArrayType Type

data Expr = ConstInt Int | Var VarName | TupleOrStruct (Maybe VarName) (NonEmpty Expr)
  deriving stock (Show, Eq)

type Program = [Statement]
data Statement
  = -- | Variable declaration, for statically typed languages or initialization for scope
    DeclareVar VarName Type
  | ReadVar
      -- | Read a single variable from stdin
      VarName
      Terminal
  | -- | Read multiple variables from stdin, separated by spaces. Having this as a single statement means more idiomatic usage of things like scanf in C.
    ReadVars
      -- | Separator string
      Text
      -- | Variable names and types
      (NonEmpty (VarName, Terminal))
  | -- | Loop over a range of numbers, from 0 to
    LoopNTimes
      -- | Loop variable name
      VarName
      -- | End
      Expr
      -- | Loop body
      [Statement]
  | -- | Array assignment
    ArrayAssign
      -- | Array variable name
      VarName
      -- | Index expression
      Expr
      -- | Value expression
      Expr

prettyExpr :: Expr -> Doc ann
prettyExpr (ConstInt i) = pretty i
prettyExpr (Var vn) = pretty vn
prettyExpr (TupleOrStruct n es) = pretty (n ?: "unnamed") <> tupled (fmap prettyExpr (toList es))

prettyVarType :: Type -> Doc ann
prettyVarType (TerminalType t) = pretty t
prettyVarType (ArrayType e t) = prettyVarType t <> brackets (prettyExpr e)
prettyVarType (DynamicArrayType t) = brackets (prettyVarType t)
prettyVarType (TupleOrStructType n ts) = pretty n <> align (encloseSep "{ " " }" ", " (fmap prettyTuple (toList ts)))
  where
    prettyTuple (n', t') = pretty n' <> ":" <+> prettyVarType t'

prettyStatement :: Statement -> Doc ann
prettyStatement (DeclareVar vn t) = pretty vn <+> ":" <+> prettyVarType t
prettyStatement (ReadVar vn t) = "read" <+> pretty vn <+> ":" <+> pretty t
prettyStatement (ReadVars sep' vars) = "read" <+> dquotes (pretty sep') <+> hsep (punctuate "," (fmap prettyVar (toList vars)))
  where
    prettyVar (vn, t) = pretty vn <> ":" <+> pretty t
prettyStatement (LoopNTimes vn end body) =
  group $
    vsep
      [ nest 2 $
          vsep
            [ "for" <+> parens (pretty vn <+> "in" <+> "0" <> ".." <> prettyExpr end) <+> "{"
            , vsep $ toList $ fmap pretty body
            ]
      , "}"
      ]
prettyStatement (ArrayAssign vn idx val) = pretty vn <> brackets (prettyExpr idx) <+> "=" <+> prettyExpr val

prettyProgram :: Program -> Doc ann
prettyProgram = vsep . fmap prettyStatement

instance Pretty Expr where
  pretty = prettyExpr

instance Pretty Statement where
  pretty = prettyStatement

instance Pretty Terminal where
  pretty StringTerminal = "String"
  pretty IntegerTerminal = "Int"
  pretty FloatTerminal = "Float"
