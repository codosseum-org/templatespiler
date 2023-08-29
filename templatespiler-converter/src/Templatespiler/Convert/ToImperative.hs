module Templatespiler.Convert.ToImperative where

import Control.Monad.Writer
import Language.Templatespiler.Syntax (Binding (..), BindingList (..), Combinator (..), Ident (..), TerminalType (..), Type (..))
import Templatespiler.IR.Common (VarName (..), withSuffix)
import Templatespiler.IR.Imperative qualified as IR
import Prelude hiding (Type)

type IRWriter a = Writer [IR.Statement] a

toIR :: BindingList -> IR.Program
toIR (BindingList bs) = execWriter $ traverse bindingToIR bs

bindingToIR :: Binding -> IRWriter IR.Expr
bindingToIR (Binding name t) = typeVarToIR name t

typeVarToIR :: Ident -> Type -> IRWriter IR.Expr
typeVarToIR name (TerminalType t) = terminalVarToIR name t
typeVarToIR name (CombinatorType c) = combinatorVarToIR name c

combinatorVarToIR :: Ident -> Combinator -> IRWriter IR.Expr
combinatorVarToIR _ (NamedCombinator newName c) = typeVarToIR newName c
combinatorVarToIR _ (GroupCombinator (BindingList bs)) = do
  es <- traverse bindingToIR bs
  pure (IR.TupleOrStruct Nothing es)
combinatorVarToIR name (ArrayCombinator len b) = do
  let lenExpr = IR.ConstInt len
  arrayLike (identToVarName name) lenExpr b
combinatorVarToIR name (SepByCombinator sep (BindingList bindingList)) = do
  let toReadAssign (Binding n t) =
        ( identToVarName n
        , case t of
            TerminalType StringType -> IR.ReadString
            TerminalType IntType -> IR.ReadInt
            TerminalType FloatType -> IR.ReadFloat
            CombinatorType c -> error ("CombinatorType in toReadAssign: " <> (show c))
        )
  let rAss = toReadAssign <$> bindingList
  tell [IR.MultiReadAssign (toText sep) rAss]
  pure (IR.TupleOrStruct (Just (identToVarName name)) (IR.Var . fst <$> rAss))
combinatorVarToIR name (ListCombinator b) = do
  let vn = identToVarName name
  let lenName = vn `withSuffix` "len"
  tell [IR.Assign lenName IR.IntType (IR.ReadAtom IR.ReadInt)]
  arrayLike vn (IR.Var lenName) b

arrayLike :: VarName -> IR.Expr -> Type -> IRWriter IR.Expr
arrayLike vn lenExpr b = do
  tell [IR.Decl vn (IR.ArrayType lenExpr (tryFigureOutTypeOf (Just lenExpr) b))]

  let idxName = vn `withSuffix` "idx"
  let (_, b') = runWriter $ do
        e <- typeVarToIR (Ident "unnamed") b
        tell [IR.AppendToArray vn (IR.Var idxName) e]

  tell [IR.For idxName (IR.ConstInt 0) lenExpr b']
  pure (IR.Var vn)

tryFigureOutTypeOf :: Maybe IR.Expr -> Type -> IR.VarType
tryFigureOutTypeOf _ (TerminalType StringType) = IR.StringType
tryFigureOutTypeOf _ (TerminalType IntType) = IR.IntType
tryFigureOutTypeOf _ (TerminalType FloatType) = IR.FloatType
tryFigureOutTypeOf _ (CombinatorType (NamedCombinator _ t)) = tryFigureOutTypeOf Nothing t
tryFigureOutTypeOf _ (CombinatorType (GroupCombinator (BindingList bs))) = IR.TupleOrStructType Nothing (fmap (\(Binding _ t) -> tryFigureOutTypeOf Nothing t) bs)
tryFigureOutTypeOf _ (CombinatorType (ArrayCombinator len t)) = IR.ArrayType (IR.ConstInt len) (tryFigureOutTypeOf Nothing t)
tryFigureOutTypeOf _ (CombinatorType (SepByCombinator _ (BindingList bs))) = IR.TupleOrStructType Nothing (fmap (\(Binding _ t) -> tryFigureOutTypeOf Nothing t) bs)
tryFigureOutTypeOf (Just len) (CombinatorType (ListCombinator t)) = IR.ArrayType len (tryFigureOutTypeOf Nothing t)
tryFigureOutTypeOf Nothing (CombinatorType (ListCombinator t)) = IR.DynamicArrayType (tryFigureOutTypeOf Nothing t)

terminalVarToIR :: Ident -> TerminalType -> IRWriter IR.Expr
terminalVarToIR name StringType = do
  tell [IR.Assign (identToVarName name) IR.StringType (IR.ReadAtom IR.ReadString)]
  pure (IR.Var (identToVarName name))
terminalVarToIR name IntType = do
  tell [IR.Assign (identToVarName name) IR.IntType (IR.ReadAtom IR.ReadInt)]
  pure (IR.Var (identToVarName name))
terminalVarToIR name FloatType = do
  tell [IR.Assign (identToVarName name) IR.FloatType (IR.ReadAtom IR.ReadFloat)]
  pure (IR.Var (identToVarName name))

identToVarName :: Ident -> VarName
identToVarName (Ident name) = VarName (name :| [])
