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
arbitrarySaneFloat = Gen.double (Range.linearFrac (-1000) 1000)

arbitraryInput :: BindingList -> Gen [Text]
arbitraryInput (BindingList bs) = join <$> traverse arbitraryBinding (toList bs)

arbitraryCombinator :: Combinator -> Gen [Text]
arbitraryCombinator (NamedCombinator _ c) = arbitraryType Nothing c
arbitraryCombinator (ArrayCombinator count g) =
  join
    <$> Gen.list
      (Range.singleton count)
      (arbitraryType Nothing g)
arbitraryCombinator (SepByCombinator sep g) = arbitraryType (Just sep) g
arbitraryCombinator (ListCombinator g) = do
  i <- Gen.int (Range.linear 1 10)
  outputs <- join <$> Gen.list (Range.singleton i) (arbitraryType Nothing g)
  pure ([show i] <> outputs)
arbitraryCombinator (GroupCombinator bs) = arbitraryInput bs

arbitraryCombinatorWithSep :: Maybe Text -> Combinator -> Gen [Text]
arbitraryCombinatorWithSep maybeSep (GroupCombinator bs) = do
  bs' <- arbitraryInput bs
  case maybeSep of
    Nothing -> pure bs'
    Just sep -> pure [Text.intercalate sep bs']
arbitraryCombinatorWithSep _ o = arbitraryCombinator o

arbitraryBinding :: Binding -> Gen [Text]
arbitraryBinding (Binding _ t) = arbitraryType Nothing t

arbitraryType :: Maybe Text -> Type -> Gen [Text]
arbitraryType _ (TerminalType t) = arbitraryTerminalType t
arbitraryType mSep (CombinatorType c) = arbitraryCombinatorWithSep mSep c

arbitraryTerminalType :: TerminalType -> Gen [Text]
arbitraryTerminalType IntType = pure . show <$> arbitrarySaneInt
arbitraryTerminalType StringType = pure . toText <$> Gen.string (Range.linear 1 20) Gen.alphaNum
arbitraryTerminalType FloatType = pure . show <$> arbitrarySaneFloat
