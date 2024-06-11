{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Templatespiler.Emit.Target where

import Prettyprinter
import Templatespiler.Convert.Target
import Templatespiler.Emit.C qualified as C
import Templatespiler.Emit.Python qualified as Py
import Templatespiler.ToLang.C qualified as C
import Templatespiler.ToLang.Python qualified as Py
import Templatespiler.ToLang.Target

class (LangAST lang ~ ast) => EmitLang (lang :: TargetLanguage) ast where
  emitLang :: LangAST lang -> Doc ()

instance EmitLang 'Python Py.Program where
  emitLang = Py.emitPy

instance EmitLang 'C C.Program where
  emitLang = C.emitC
