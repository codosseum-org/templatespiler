module Language.Templatespiler.Syntax where

import Data.List.NonEmpty (NonEmpty)
import Data.Text
import Prelude hiding (Type)

newtype BindingList = BindingList (NonEmpty Binding)
  deriving (Show)

newtype Ident = Ident Text deriving (Show)

data Binding = Binding Ident Type deriving (Show)

data Type = TerminalType TerminalType | CombinatorType Combinator deriving (Show)

data TerminalType = IntType | FloatType | StringType deriving (Show)

data Combinator
  = NamedCombinator Ident Type
  | GroupCombinator BindingList
  | ArrayCombinator Int Type
  | SepByCombinator Text Type
  | ListCombinator Type
  deriving (Show)
