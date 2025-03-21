{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Templatespiler.Convert where

import Language.Templatespiler.Syntax
import Prettyprinter
import Prettyprinter.Render.Terminal
import Templatespiler.Convert.Target
import Templatespiler.Emit.Common
import Templatespiler.Emit.Target
import Templatespiler.ToLang.Target

convertTo :: BindingList -> TargetLanguage -> Maybe ConvertResult
convertTo bindingList lang = case lang of
  Python -> Just $ convertTo' @Python bindingList
  C -> Just $ convertTo' @C bindingList
  Haskell -> Just $ convertTo' @Haskell bindingList

convertTo' ::
  forall (target :: TargetLanguage) ast.
  (ToIR target (ParadigmOf target), ToLang target ast, EmitLang target ast) =>
  BindingList ->
  ConvertResult
convertTo' bindingList = do
  let ir = toIR @target bindingList
  let ast = toLang @target ir
  emitLang @target ast

renderConvertResult :: ConvertResult -> Text
renderConvertResult (ConversionFailed doc) = renderStrict $ layoutPretty defaultLayoutOptions doc
renderConvertResult (ConvertResult warnings code) = renderStrict $ layoutPretty defaultLayoutOptions $ vsep warnings <> reAnnotate (const mempty) code
