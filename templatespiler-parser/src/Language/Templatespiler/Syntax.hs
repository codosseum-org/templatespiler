module Language.Templatespiler.Syntax where

import Data.List.NonEmpty (NonEmpty)
import Data.Text
import Prelude hiding (Type)

newtype BindingList = BindingList (NonEmpty Binding)
  deriving (Show, Eq)

newtype Ident = Ident Text deriving (Show, Eq)

data Binding = Binding Ident Type deriving (Show, Eq)

data Type
  = TerminalType TerminalType
  | CombinatorType Combinator
  deriving (Show, Eq)

-- | The terminal / atomic types that can be used in a binding.
data TerminalType
  = -- | An integer.
    IntType
  | -- | A floating point number.
    FloatType
  | -- | A string.
    StringType
  deriving (Show, Eq)

data Combinator
  = -- | A named combinator, e.g. @foo: int@.
    NamedCombinator Ident Type
  | -- | A group of bindings, eg @[x : Integer, y : Integer]@
    GroupCombinator BindingList
  | -- | An array of a given length, eg @array 3 (num : Integer)@
    ArrayCombinator Int Type
  | -- | A list of bindings separated by a given string, eg @sep-by " " [x : Integer, y : Integer]@
    SepByCombinator Text BindingList
  | -- | A list (dynamic array) of a given type, eg @list (num : Integer)@
    ListCombinator Type
  deriving (Show, Eq)
