{-# LANGUAGE LexicalNegation #-}

module Templatespiler.Generate where

import Data.Text qualified as Text
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Language.Templatespiler.Syntax
import Prelude hiding (Type)

arbitrarySaneInt :: Gen Int
arbitrarySaneInt = Gen.int (Range.linear -1000 1000)

arbitrarySaneFloat :: Gen Double
arbitrarySaneFloat = Gen.double (Range.linearFrac -1000 1000)

arbitraryInput :: BindingList -> Gen [Text]
arbitraryInput (BindingList bs) = join <$> traverse arbitraryBinding (toList bs)

arbitraryCombinator :: Combinator -> Gen [Text]
arbitraryCombinator (NamedCombinator _ c) = arbitraryType c
arbitraryCombinator (ArrayCombinator count g) =
  join
    <$> Gen.list
      (Range.singleton count)
      (arbitraryBindingOrCombinator g)
arbitraryCombinator (SepByCombinator sep bs) = do
  bs' <- arbitraryInput bs
  pure [Text.intercalate sep bs']
arbitraryCombinator (ListCombinator g) = do
  i <- Gen.int (Range.linear 1 10)
  outputs <- join <$> Gen.list (Range.singleton i) (arbitraryBindingOrCombinator g)
  pure ([show i] <> outputs)
arbitraryCombinator (GroupCombinator bs) = arbitraryInput bs

arbitraryBinding :: Binding -> Gen [Text]
arbitraryBinding (Binding _ t) = arbitraryType t

arbitraryType :: Type -> Gen [Text]
arbitraryType (TerminalType t) = arbitraryTerminalType t
arbitraryType (CombinatorType c) = arbitraryCombinator c

arbitraryBindingOrCombinator :: BindingOrCombinator -> Gen [Text]
arbitraryBindingOrCombinator (NamedBinding b) = arbitraryBinding b
arbitraryBindingOrCombinator (GroupBinding bs) = arbitraryInput bs
arbitraryBindingOrCombinator (UnnamedBinding c) = arbitraryCombinator c

arbitraryTerminalType :: TerminalType -> Gen [Text]
arbitraryTerminalType IntType = pure . show <$> arbitrarySaneInt
arbitraryTerminalType StringType = pure . toText <$> Gen.string (Range.linear 1 20) Gen.alphaNum
arbitraryTerminalType FloatType = pure . show <$> arbitrarySaneFloat
