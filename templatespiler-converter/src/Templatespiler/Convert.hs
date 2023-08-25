{-# LANGUAGE ViewPatterns #-}

module Templatespiler.Convert where

import Control.Monad.Writer
import Language.Templatespiler.Abs
import Shower (shower)
import Templatespiler.IR.Imperative (withSuffix)
import Templatespiler.IR.Imperative qualified as Imp
import Prelude hiding (Type)

toImperative :: BindingList -> [Imp.Statement]
toImperative (BindingList _ bs) = execWriter $ traverse toImperativeBinding bs
  where
    toImperativeBinding :: Binding -> Writer [Imp.Statement] Imp.Expr
    toImperativeBinding (Binding _ n t) = do
      case t of
        StringType _ -> do
          tell [Imp.Assign (toVarName n) Imp.StringType (Imp.ReadAtom Imp.ReadString)]
          pure (Imp.Var (toVarName n))
        IntegerType _ -> do
          tell [Imp.Assign (toVarName n) Imp.IntType (Imp.ReadAtom Imp.ReadInt)]
          pure (Imp.Var (toVarName n))
        FloatType _ -> do
          tell [Imp.Assign (toVarName n) Imp.FloatType (Imp.ReadAtom Imp.ReadFloat)]
          pure (Imp.Var (toVarName n))
        CombinatorType _ combin -> do
          toImperativeCombinator n combin

    toImperativeBindingOrCombinator :: BindingOrCombinator -> Writer [Imp.Statement] Imp.Expr
    toImperativeBindingOrCombinator (NamedBinding _ b) = toImperativeBinding b
    toImperativeBindingOrCombinator (ParenBinding _ b) = toImperativeBinding b
    toImperativeBindingOrCombinator (UnnamedBinding _ c) = toImperativeCombinator (Ident "unnamed") c
    toImperativeBindingOrCombinator (GroupBinding _ (BindingGroup _ bs)) = do
      es <- traverse toImperativeBinding bs
      pure (Imp.TupleOrStruct Nothing (fromList es))

    toImperativeCombinator :: Ident -> Combinator' BNFC'Position -> Writer [Imp.Statement] Imp.Expr
    toImperativeCombinator n (ParenCombinator _ c) = toImperativeCombinator n c
    toImperativeCombinator n (SepByCombinator _ sep c) = do
      let (BindingGroup _ (fromList @(NonEmpty _) -> bs)) = c
      let toReadAssign (Binding _ n t) =
            ( toVarName n
            , case t of
                StringType _ -> Imp.ReadString
                IntegerType _ -> Imp.ReadInt
                FloatType _ -> Imp.ReadFloat
                CombinatorType _ c -> error ("CombinatorType in toReadAssign: " <> toText (shower c))
            )
      let rAss = toReadAssign <$> bs
      tell [Imp.MultiReadAssign (toText sep) rAss]
      pure (Imp.TupleOrStruct (Just (toVarName n)) (Imp.Var . fst <$> rAss))
    toImperativeCombinator n (ListCombinator _ b) = do
      let vn = toVarName n
      let lenName = vn `withSuffix` "len"
      tell [Imp.Assign lenName Imp.IntType (Imp.ReadAtom Imp.ReadInt)]
      arrayLike vn (Imp.Var lenName) b
    toImperativeCombinator n (ArrayCombinator _ len b) = do
      let lenExpr = case len of
            ConstInt _ i -> Imp.ConstInt (fromInteger i)
            ConstVar _ v -> Imp.Var (toVarName v)
      arrayLike (toVarName n) lenExpr b

    arrayLike vn lenExpr b = do
      tell [Imp.Decl vn (Imp.ArrayType lenExpr (tryFigureOutTypeOf b))]

      let idxName = vn `withSuffix` "idx"
      let (_, b') = runWriter $ do
            e <- toImperativeBindingOrCombinator b
            tell [Imp.AppendToArray vn (Imp.Var idxName) e]

      tell [Imp.For idxName (Imp.ConstInt 0) lenExpr b']
      pure (Imp.Var vn)

toVarName :: Ident -> Imp.VarName
toVarName (Ident n) = toVarName' n

toVarName' :: (ToText s) => s -> Imp.VarName
toVarName' s = Imp.VarName (toText s :| [])

tryFigureOutTypeOf :: BindingOrCombinator -> Imp.VarType
tryFigureOutTypeOf (NamedBinding _ (Binding _ _ t)) = toVarType t
tryFigureOutTypeOf (ParenBinding _ (Binding _ _ t)) = toVarType t
tryFigureOutTypeOf (UnnamedBinding _ _) = Imp.UnknownType
tryFigureOutTypeOf (GroupBinding _ _) = Imp.UnknownType

toVarType :: Type -> Imp.VarType
toVarType (StringType _) = Imp.StringType
toVarType (IntegerType _) = Imp.IntType
toVarType (FloatType _) = Imp.FloatType
toVarType (CombinatorType _ _) = Imp.UnknownType
