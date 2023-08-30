{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Templatespiler.Convert where

import Language.Templatespiler.Syntax
import Prettyprinter
import Prettyprinter.Render.Text
import Templatespiler.Convert.Target
import Templatespiler.Emit.Target
import Templatespiler.ToLang.Target

convertTo :: BindingList -> TargetLanguage -> Maybe Text
convertTo bindingList lang = case lang of
  Python -> Just $ convertTo' @Python bindingList
  _ -> Nothing

convertTo' ::
  forall (target :: TargetLanguage) ast.
  (ToIR target (ParadigmOf target), ToLang target ast, EmitLang target ast) =>
  BindingList ->
  Text
convertTo' bindingList = do
  let ir = toIR @target bindingList :: IRTarget (ParadigmOf target)
  let ast = toLang @target @ast ir
  let doc = emitLang @target @ast ast
  renderStrict $ layoutPretty defaultLayoutOptions doc
