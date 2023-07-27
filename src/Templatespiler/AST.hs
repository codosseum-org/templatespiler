module Templatespiler.AST where

type Block = NonEmpty Statement

data Statement
  = VariableDecl Binder Expression
  | Yield Binder
  | PrintLn Text
  deriving (Show)

data Binder
  = Binder (NonEmpty Text)
  deriving (Show)

data Expression
  = ReadLn ReadType
  | -- | for {count} as {var} {block}
    For Text Text Block
  deriving (Show)

data ReadType
  = ReadWords (NonEmpty ReadType)
  | ReadInteger
  | ReadLong
  | ReadString
  deriving (Show)