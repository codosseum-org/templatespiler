{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Templatespiler.Parser
import Language.Templatespiler.Pretty
import Language.Templatespiler.Syntax

import Hedgehog

import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Data.Set qualified as Set
import Hedgehog.Internal.Property (failWith)
import Prettyprinter (defaultLayoutOptions, layoutPretty, unAnnotate)
import Prettyprinter.Render.String
import Prettyprinter.Render.Terminal (renderStrict)
import Text.Trifecta (ErrInfo (_errDoc), Result (..), parseString)
import Prelude hiding (Type)

arbitraryBindingList :: Gen BindingList
arbitraryBindingList = Gen.sized $ \size ->
  BindingList <$> Gen.nonEmpty (Range.linear 1 (Range.unSize size)) arbitraryBinding

arbitraryBinding :: Gen Binding
arbitraryBinding = Binding <$> arbitraryIdent <*> arbitraryType

keywords :: Set Text
keywords = Set.fromList ["list", "array", "sep-by"]

arbitraryIdent :: Gen Ident
arbitraryIdent =
  Ident
    <$> Gen.filter
      (`Set.notMember` keywords)
      ( do
          first <- Gen.lower
          rest <- Gen.string (Range.linear 0 10) Gen.alphaNum
          pure $ toText (first : rest)
      )

arbitraryType :: Gen Type
arbitraryType = Gen.sized $ \size ->
  if size <= 1
    then TerminalType <$> arbitraryTerminalType
    else
      Gen.choice
        [ TerminalType <$> arbitraryTerminalType
        , CombinatorType <$> Gen.small arbitraryCombinator
        ]

arbitraryTerminalType :: Gen TerminalType
arbitraryTerminalType = Gen.element [IntType, FloatType, StringType]

arbitraryCombinator :: Gen Combinator
arbitraryCombinator =
  Gen.choice
    -- [GroupCombinator <$> arbitraryBindingList]
    [ NamedCombinator <$> arbitraryIdent <*> arbitraryType
    , ListCombinator <$> Gen.small arbitraryBindingOrCombinator
    , ArrayCombinator <$> Gen.integral (Range.linear 1 10) <*> Gen.small arbitraryBindingOrCombinator
    , SepByCombinator <$> arbitrarySep <*> arbitraryBindingList
    ]

arbitraryBindingOrCombinator :: Gen BindingOrCombinator
arbitraryBindingOrCombinator = Gen.sized $ \size ->
  if size <= 1
    then NamedBinding <$> arbitraryBinding
    else
      Gen.choice
        [ NamedBinding <$> arbitraryBinding
        , GroupBinding <$> Gen.small arbitraryBindingList
        , UnnamedBinding <$> Gen.small arbitraryCombinator
        ]

arbitrarySep :: Gen Text
arbitrarySep = Gen.text (Range.linear 1 10) Gen.alpha

prop_pprParse :: Property
prop_pprParse = withTests 1000 $ property $ do
  bindingList <- forAll arbitraryBindingList
  let showPrettyUnannotated = renderString . layoutPretty defaultLayoutOptions . unAnnotate . prettyBindingList

  trippingTrifecta bindingList showPrettyUnannotated (parseString parseTemplateProgram mempty)

trippingTrifecta x encode decode = do
  let i = encode x
  let my = decode i
  case my of
    Failure e -> do
      footnoteShow i
      withFrozenCallStack $ failWith Nothing $ toString $ renderStrict $ layoutPretty defaultLayoutOptions (_errDoc e)
    Success y -> tripping x (const i) (const (Identity y))

main :: IO ()
main = unlessM (checkParallel $$discover) exitFailure
