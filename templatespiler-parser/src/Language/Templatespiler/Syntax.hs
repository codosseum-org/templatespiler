module Language.Templatespiler.Syntax where

import Data.List.NonEmpty (NonEmpty)
import Data.Text
import Prelude hiding (Type)

newtype BindingList = BindingList (NonEmpty Binding)
  deriving (Show, Eq, Ord)

newtype Ident = Ident Text deriving (Show, Eq, Ord)

data Binding = Binding Ident Type deriving (Show, Eq, Ord)

bindingType :: Binding -> Type
bindingType (Binding _ t) = t

data Type
  = TerminalType TerminalType
  | CombinatorType Combinator
  deriving (Show, Eq, Ord)

-- | The terminal / atomic types that can be used in a binding.
data TerminalType
  = -- | An integer.
    IntType
  | -- | A floating point number.
    FloatType
  | -- | A string.
    StringType
  deriving (Show, Eq, Ord)

data Combinator
  = -- | A named combinator, e.g. @foo: int@.
    NamedCombinator Ident Type
  | -- \| An array of a given length, eg @array 3 (num : Integer)@

    -- | -- | A group of bindings, eg @[x : Integer, y : Integer]@
    --   GroupCombinator BindingList
    ArrayCombinator Int BindingOrCombinator
  | -- | A list of bindings separated by a given delimiter, eg @sep-by " " [x : Integer, y : Integer]@
    -- This directly correlates to reading a single line of input and splitting it by the given delimiter
    -- and then parsing each element as the given type.
    SepByCombinator Text BindingList
  | -- | A list (dynamic array) of a given type, eg @list (num : Integer)@
    ListCombinator BindingOrCombinator
  deriving (Show, Eq, Ord)

data BindingOrCombinator
  = NamedBinding Binding
  | GroupBinding BindingList
  | UnnamedBinding Combinator
  deriving (Show, Eq, Ord)
