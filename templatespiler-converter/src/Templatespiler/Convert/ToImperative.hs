module Templatespiler.Convert.ToImperative where

import Language.Templatespiler.Syntax (BindingList (..))
import Templatespiler.IR.Imperative as IR

toIR :: BindingList -> IR.Program
toIR (BindingList bs) = []
