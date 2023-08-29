module Language.Templatespiler.Syntax where

import Data.List.NonEmpty (NonEmpty)
import Data.Text
import Prelude hiding (Type)

newtype BindingList = BindingList (NonEmpty Binding)
  deriving (Show, Eq)

newtype Ident = Ident Text deriving (Show, Eq)

data Binding = Binding Ident Type deriving (Show, Eq)

data Type = TerminalType TerminalType | CombinatorType Combinator deriving (Show, Eq)

data TerminalType = IntType | FloatType | StringType deriving (Show, Eq)

data Combinator
  = NamedCombinator Ident Type
  | GroupCombinator BindingList
  | ArrayCombinator Int Type
  | SepByCombinator Text BindingList
  | ListCombinator Type
  deriving (Show, Eq)
