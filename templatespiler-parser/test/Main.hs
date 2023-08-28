{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Templatespiler.Parser
import Language.Templatespiler.Pretty
import Language.Templatespiler.Syntax

import Hedgehog

import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Hedgehog.Internal.Property (failWith)
import Prettyprinter (defaultLayoutOptions, layoutPretty, unAnnotate)
import Prettyprinter.Render.String
import Prettyprinter.Render.Terminal (renderStrict)
import Text.Trifecta (ErrInfo (_errDoc), Result (..), parseString)
import Prelude hiding (Type)

arbitraryBindingList :: Gen BindingList
arbitraryBindingList = BindingList <$> Gen.nonEmpty (Range.linear 1 10) arbitraryBinding

arbitraryBinding :: Gen Binding
arbitraryBinding = Binding <$> arbitraryIdent <*> arbitraryType

arbitraryIdent :: Gen Ident
arbitraryIdent = Ident <$> Gen.text (Range.linear 1 10) Gen.alpha

arbitraryType :: Gen Type
arbitraryType =
  Gen.recursive
    Gen.choice
    [TerminalType <$> arbitraryTerminalType]
    [CombinatorType <$> arbitraryCombinator]

arbitraryTerminalType :: Gen TerminalType
arbitraryTerminalType = Gen.element [IntType, FloatType, StringType]

arbitraryCombinator :: Gen Combinator
arbitraryCombinator =
  Gen.recursive
    Gen.choice
    [GroupCombinator <$> arbitraryBindingList]
    [ NamedCombinator <$> arbitraryIdent <*> arbitraryType
    , ListCombinator <$> arbitraryType
    , ArrayCombinator <$> Gen.integral (Range.linear 1 10) <*> arbitraryType
    , SepByCombinator <$> arbitrarySep <*> arbitraryType
    ]

arbitrarySep :: Gen Text
arbitrarySep = Gen.text (Range.linear 1 10) Gen.alpha

prop_pprParse :: Property
prop_pprParse = property $ do
  bindingList <- forAll arbitraryBindingList
  let showPrettyUnannotated = renderString . layoutPretty defaultLayoutOptions . unAnnotate . prettyBindingList
  trippingTrifecta bindingList showPrettyUnannotated (parseString parseBindingList mempty)

trippingTrifecta x encode decode = do
  let i = encode x
  let my = decode i
  case my of
    Failure e -> do
      footnoteShow i
      withFrozenCallStack $ failWith Nothing $ toString $ renderStrict $ layoutPretty defaultLayoutOptions (_errDoc e)
    Success y -> tripping x (const i) (const (Identity y))

main :: IO ()
main = unlessM (checkSequential $$(discover)) exitFailure
