{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module Templatespiler.Emit.Target where

import Templatespiler.Convert.Target
import Templatespiler.Emit.C qualified as C
import Templatespiler.Emit.Common (ConvertResult)
import Templatespiler.Emit.Haskell qualified as Haskell
import Templatespiler.Emit.Python qualified as Py
import Templatespiler.ToLang.C qualified as C
import Templatespiler.ToLang.Haskell qualified as Haskell
import Templatespiler.ToLang.Python qualified as Py
import Templatespiler.ToLang.Target

class (LangAST lang ~ ast) => EmitLang (lang :: TargetLanguage) ast | lang -> ast where
  emitLang :: LangASTRes lang -> ConvertResult

instance EmitLang 'Python Py.Program where
  emitLang = Py.emitPyResult

instance EmitLang 'C C.Program where
  emitLang = C.emitCResult

instance EmitLang 'Haskell Haskell.HaskellProgram where
  emitLang = Haskell.emitHaskellResult
