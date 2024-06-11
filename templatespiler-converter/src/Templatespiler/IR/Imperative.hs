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

-- import Templatespiler.IR.Common (SingleLineString)
import Prelude hiding (Type)

newtype VarName = VarName Text

data Terminal
  = StringTerminal
  | IntegerTerminal
  | FloatTerminal

data Type
  = TerminalType Terminal
  | -- | An "array", which may be a list or a fixed size array depending on target language. The length must be known at compile time.
    ArrayType
      VarName
      Type

type Program = [Statement]
data Statement
  = -- | Variable declaration, for statically typed languages or initialization for scope
    DeclareVar VarName Type
  | -- | Read one or more variables from stdin, separated by spaces. Having this as a single statement means more idiomatic usage of things like scanf in C.
    ReadVars
      Text
      -- ^ Separator string
      (NonEmpty (VarName, Type))
      -- ^ Variable names and types
  | LoopNTimes
      VarName
      [Statement]
  | -- | Array assignment
    ArrayAssign
      VarName
      -- ^ Array variable name
      VarName
      -- ^ Index variable name
      VarName
      -- ^ Value variable name
