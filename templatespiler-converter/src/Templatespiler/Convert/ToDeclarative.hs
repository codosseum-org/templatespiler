module Templatespiler.Convert.ToDeclarative where

import Language.Templatespiler.Syntax (BindingList (..))
import Templatespiler.IR.Declarative as IR

toIR :: BindingList -> IR.Program
toIR (BindingList bs) = []
