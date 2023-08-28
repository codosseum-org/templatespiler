{-# LANGUAGE DerivingStrategies #-}

-- | A lightweight imperative language that is used as an IR
module Templatespiler.IR.Imperative where

import Templatespiler.IR.Common

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
  | MultiReadAssign
      Text -- Separator
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
  deriving stock (Show)
