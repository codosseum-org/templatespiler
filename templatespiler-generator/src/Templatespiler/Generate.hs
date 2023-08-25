module Templatespiler.Generate where

import Data.Text qualified as Text
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.Templatespiler.Abs (Binding, Binding' (Binding), BindingGroup, BindingGroup' (..), BindingList, BindingList' (..), BindingOrCombinator, BindingOrCombinator' (..), Combinator, Combinator' (..), Type, Type' (..), VarOrConstInt' (..))
import Prelude hiding (Type)

arbitraryInput :: BindingList -> Gen [Text]
arbitraryInput (BindingList _ bs) = join <$> traverse arbitraryBinding bs

arbitraryCombinator :: Combinator -> Gen [Text]
arbitraryCombinator (ParenCombinator _ g) = arbitraryCombinator g
arbitraryCombinator (ArrayCombinator _ (ConstInt _ count) g) =
  join
    <$> Gen.list
      (Range.singleton (fromInteger count))
      (arbitraryBindingOrCombinator g)
arbitraryCombinator (ArrayCombinator _ (ConstVar _ _) _) = error "ConstVar in ArrayCombinator"
arbitraryCombinator (SepByCombinator _ sep g) = arbitraryBindingGroup sep g
arbitraryCombinator (ListCombinator _ g) = do
  i <- Gen.int (Range.linear 1 10)
  outputs <- join <$> Gen.list (Range.singleton i) (arbitraryBindingOrCombinator g)
  pure ([show i] <> outputs)

arbitraryBindingOrCombinator :: BindingOrCombinator -> Gen [Text]
arbitraryBindingOrCombinator (NamedBinding _ b) = arbitraryBinding b
arbitraryBindingOrCombinator (GroupBinding _ (BindingGroup _ bs)) = do
  join <$> traverse arbitraryBinding bs
arbitraryBindingOrCombinator (ParenBinding _ b) = arbitraryBinding b
arbitraryBindingOrCombinator (UnnamedBinding _ c) = arbitraryCombinator c

arbitraryBindingGroup :: String -> BindingGroup -> Gen [Text]
arbitraryBindingGroup sep (BindingGroup _ bs) = do
  bs' <- join <$> traverse arbitraryBinding bs
  pure [Text.intercalate (toText sep) bs']

arbitraryBinding :: Binding -> Gen [Text]
arbitraryBinding (Binding _ n t) = do
  arbitraryType t

arbitraryType :: Type -> Gen [Text]
arbitraryType (IntegerType _) = pure . show <$> Gen.int Range.linearBounded
arbitraryType (StringType _) = pure . toText <$> Gen.string (Range.linear 1 20) Gen.alphaNum
arbitraryType (FloatType _) = pure . show <$> Gen.double (fromIntegral <$> Range.linearBounded @Int)
arbitraryType (CombinatorType _ c) = arbitraryCombinator c
