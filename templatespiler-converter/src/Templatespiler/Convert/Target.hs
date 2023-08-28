{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Templatespiler.Convert.Target where

import Language.Templatespiler.Syntax (BindingList)
import Templatespiler.Convert.ToDeclarative as Declarative (toIR)
import Templatespiler.Convert.ToImperative as Imperative (toIR)
import Templatespiler.IR.Declarative as DecIR
import Templatespiler.IR.Imperative as ImpIR

data TargetLanguage = C | Python | Haskell

data LanguageKind = Imperative | Declarative

type family LanguageKindOf (lang :: TargetLanguage) = p where
  LanguageKindOf C = Imperative
  LanguageKindOf Python = Imperative
  LanguageKindOf Haskell = Declarative

type family IRTarget (lang :: LanguageKind) = p | p -> lang where
  IRTarget Imperative = ImpIR.Program
  IRTarget Declarative = DecIR.Program

class (ir ~ LanguageKindOf lang) => ToIR lang ir where
  toIR :: BindingList -> IRTarget ir

instance (LanguageKindOf lang ~ Imperative) => ToIR lang Imperative where
  toIR = Imperative.toIR

instance (LanguageKindOf lang ~ Declarative) => ToIR lang Declarative where
  toIR = Declarative.toIR
