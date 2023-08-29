{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Templatespiler.ToLang.Target where

import Templatespiler.ToLang.Python as Py

import Templatespiler.Convert.Target (IRTarget, ParadigmOf, TargetLanguage (..))

type family LangAST (lang :: TargetLanguage) where
  LangAST 'Python = Py.Program

class (LangAST lang ~ ast) => ToLang (lang :: TargetLanguage) ast where
  toLang :: IRTarget (ParadigmOf lang) -> ast

instance ToLang 'Python Py.Program where
  toLang = fst . Py.toPython
