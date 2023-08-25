module Templatespiler.Convert where

import Control.Monad.Writer
import Language.Templatespiler.Abs
import Templatespiler.IR.Imperative as Imp

toImperative :: BindingList -> [Imp.Statement]
toImperative (BindingList _ bs) = foldMap toImperativeBinding bs
  where
    toImperativeBinding :: Binding -> [Imp.Statement]
    toImperativeBinding (Binding _ n t) = execWriter $ do
      case t of
        StringType _ -> do
          tell [Decl n (Read n [ReadString])]
        IntegerType _ -> do
          tell [Decl n (Read n [ReadInt])]
        FloatType _ -> do
          tell [Decl n (Read n [ReadFloat])]
        CombinatorType _ combin -> do
          toImperativeCombinator n combin

    toImperativeCombinator :: Ident -> Combinator' BNFC'Position -> Writer [Statement] ()
    toImperativeCombinator n (ListCombinator b) = do
      tell [Decl ()]
