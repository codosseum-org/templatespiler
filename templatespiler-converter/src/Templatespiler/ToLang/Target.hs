{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Templatespiler.ToLang.Target where

import Templatespiler.ToLang.C qualified as C
import Templatespiler.ToLang.Haskell qualified as Haskell
import Templatespiler.ToLang.Python qualified as Py

import Templatespiler.Convert.Target (IRTarget, ParadigmOf, TargetLanguage (..))

type family LangAST (lang :: TargetLanguage) where
  LangAST 'Python = Py.Program
  LangAST 'C = C.Program
  LangAST 'Haskell = Haskell.HaskellProgram

type family LangASTRes (lang :: TargetLanguage) where
  LangASTRes 'Python = (Py.Program, [Py.ToPythonWarning])
  LangASTRes 'C = Either C.ToCError (C.Program, [C.ToCWarning])
  LangASTRes 'Haskell = (Haskell.HaskellProgram, [Haskell.ToHaskellWarning])

class (LangAST lang ~ ast) => ToLang (lang :: TargetLanguage) ast | lang -> ast where
  toLang :: IRTarget (ParadigmOf lang) -> LangASTRes lang

instance ToLang 'Python Py.Program where
  toLang = Py.toPython

instance ToLang 'C C.Program where
  toLang = C.toC

instance ToLang 'Haskell Haskell.HaskellProgram where
  toLang = Haskell.toHaskell
