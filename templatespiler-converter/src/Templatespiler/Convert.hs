module Templatespiler.Convert where

import Control.Monad.Writer
import Language.Templatespiler.Abs
import Shower (shower)
import Templatespiler.IR.Imperative (withSuffix)
import Templatespiler.IR.Imperative qualified as Imp

toImperative :: BindingList -> [Imp.Statement]
toImperative (BindingList _ bs) = foldMap toImperativeBinding bs
  where
    toImperativeBinding :: Binding -> [Imp.Statement]
    toImperativeBinding (Binding _ n t) = execWriter $ do
      case t of
        StringType _ -> do
          tell [Imp.Assign (toVarName n) Imp.StringType (Imp.ReadAtom Imp.ReadString)]
        IntegerType _ -> do
          tell [Imp.Assign (toVarName n) Imp.IntType (Imp.ReadAtom Imp.ReadInt)]
        FloatType _ -> do
          tell [Imp.Assign (toVarName n) Imp.FloatType (Imp.ReadAtom Imp.ReadFloat)]
        CombinatorType _ combin -> do
          toImperativeCombinator n combin

    toImperativeBindingOrCombinator :: BindingOrCombinator -> Writer [Imp.Statement] ()
    toImperativeBindingOrCombinator (NamedBinding _ b) = tell $ toImperativeBinding b
    toImperativeBindingOrCombinator (ParenBinding _ b) = tell $ toImperativeBinding b
    toImperativeBindingOrCombinator (UnnamedBinding _ c) = toImperativeCombinator (Ident "unnamed") c
    toImperativeBindingOrCombinator (GroupBinding _ (BindingGroup _ bs)) = do
      tell $ foldMap toImperativeBinding bs

    toImperativeCombinator :: Ident -> Combinator' BNFC'Position -> Writer [Imp.Statement] ()
    toImperativeCombinator n (ParenCombinator _ c) = toImperativeCombinator n c
    toImperativeCombinator n (SepByCombinator _ sep c) = do
      let (BindingGroup _ bs) = c
      let toReadAssign (Binding _ n t) =
            ( toVarName n
            , case t of
                StringType _ -> Imp.ReadString
                IntegerType _ -> Imp.ReadInt
                FloatType _ -> Imp.ReadFloat
                CombinatorType _ c -> error ("CombinatorType in toReadAssign: " <> toText (shower c))
            )
      tell [Imp.MultiReadAssign (toText sep) (toReadAssign <$> bs)]
    toImperativeCombinator n (ListCombinator _ b) = do
      let lenName = toVarName n `withSuffix` "len"
      tell [Imp.Assign lenName Imp.IntType (Imp.ReadAtom Imp.ReadInt)]
      tell [Imp.Decl (toVarName n) (Imp.ArrayType Imp.UnknownType)]
      let (_, b') = runWriter $ toImperativeBindingOrCombinator b
      tell [Imp.For (toVarName "i") (Imp.ConstInt 0) (Imp.Var lenName) b']

toVarName :: Ident -> Imp.VarName
toVarName (Ident n) = toVarName' n

toVarName' :: (ToText s) => s -> Imp.VarName
toVarName' s = Imp.VarName (toText s :| [])
