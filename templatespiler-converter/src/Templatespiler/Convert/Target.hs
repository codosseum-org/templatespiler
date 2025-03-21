{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Templatespiler.Convert.Target where

import Language.Templatespiler.Syntax (BindingList)
import Templatespiler.Convert.ToDeclarative qualified as Declarative
import Templatespiler.Convert.ToImperative qualified as Imperative
import Templatespiler.IR.Declarative as DecIR
import Templatespiler.IR.Imperative as ImpIR

data TargetLanguage = C | Python | Haskell deriving stock (Eq, Show, Ord, Generic, Enum, Bounded)

data LanguageKind = Imperative | Declarative

type family ParadigmOf (lang :: TargetLanguage) where
  ParadigmOf C = Imperative
  ParadigmOf Python = Imperative
  ParadigmOf Haskell = Declarative

type family IRTarget (lang :: LanguageKind) = p | p -> lang where
  IRTarget Imperative = ImpIR.Program
  IRTarget Declarative = BindingList -- For now, we don't have a distinct IR for declarative languages

class ToIR lang paradigm where
  toIR :: BindingList -> IRTarget paradigm

instance (ParadigmOf lang ~ Imperative) => ToIR lang Imperative where
  toIR = Imperative.toIR

instance (ParadigmOf lang ~ Declarative) => ToIR lang Declarative where
  toIR = identity
