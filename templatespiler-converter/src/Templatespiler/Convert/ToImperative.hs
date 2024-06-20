{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Templatespiler.Convert.ToImperative where

import Control.Monad.Writer
import Language.Templatespiler.Syntax (Binding (..), BindingList (..), BindingOrCombinator (..), Combinator (..), Ident (..), TerminalType (..), Type (..))
import Templatespiler.IR.Common (VarName (..), generateStructName, withSuffix)
import Templatespiler.IR.Imperative qualified as IR
import Prelude hiding (Type)

type IRWriter a = Writer [IR.Statement] a

toIR :: BindingList -> IR.Program
toIR (BindingList bs) = execWriter $ traverse bindingToIR bs

bindingToIR :: Binding -> IRWriter IR.Expr
bindingToIR (Binding name t) = readNamedVariable name t

readBindingOrCombinator :: BindingOrCombinator -> IRWriter IR.Expr
readBindingOrCombinator (NamedBinding b) = bindingToIR b
readBindingOrCombinator (GroupBinding b@(BindingList bs)) = do
  vars <- traverse bindingToIR bs
  pure (IR.TupleOrStruct (generateStructName b) vars)
readBindingOrCombinator (UnnamedBinding c) =
  combinatorVarToIR
    (findCombinatorName c)
    c

findBOrCombinatorName :: BindingOrCombinator -> VarName
findBOrCombinatorName (NamedBinding (Binding n _)) = identToVarName n
findBOrCombinatorName (GroupBinding bs) = generateStructName bs
findBOrCombinatorName (UnnamedBinding c) = findCombinatorName c
findCombinatorName :: Combinator -> VarName
findCombinatorName = \case
  NamedCombinator n _ -> identToVarName n
  ArrayCombinator _ c -> findBOrCombinatorName c
  SepByCombinator _ c -> generateStructName c
  ListCombinator c -> findBOrCombinatorName c

-- | Generates code to read a given type, store it in a variable, and return the variable.
readNamedVariable :: Ident -> Type -> IRWriter IR.Expr
readNamedVariable name (TerminalType t) = terminalVarToIR name t
readNamedVariable name (CombinatorType c) = combinatorVarToIR (identToVarName name) c

combinatorVarToIR :: VarName -> Combinator -> IRWriter IR.Expr
combinatorVarToIR _ (NamedCombinator newName c) = readNamedVariable newName c
combinatorVarToIR name (ArrayCombinator len b) = do
  let lenExpr = IR.ConstInt len
  arrayLike name lenExpr b
combinatorVarToIR name (SepByCombinator sep (BindingList bindingList)) = do
  let toReadAssign (Binding n t) =
        ( identToVarName n
        , case t of
            TerminalType t -> terminalTypeToIR t
            CombinatorType c -> error ("CombinatorType in toReadAssign: " <> show c)
        )
  let rAss = toReadAssign <$> bindingList
  tell [IR.ReadVars (toText sep) rAss]
  pure (IR.TupleOrStruct name (IR.Var . fst <$> rAss))
combinatorVarToIR name (ListCombinator b) = do
  let vn = name
  let lenName = vn `withSuffix` "len"
  tell [IR.DeclareVar lenName (IR.TerminalType IR.IntegerTerminal), IR.ReadVar lenName IR.IntegerTerminal]
  arrayLike vn (IR.Var lenName) b

arrayLike :: VarName -> IR.Expr -> BindingOrCombinator -> IRWriter IR.Expr
arrayLike vn lenExpr b = do
  -- declare the array variable
  tell [IR.DeclareVar vn (IR.ArrayType lenExpr (tryFigureOutTypeOf (Just lenExpr) b))]

  let idxName = vn `withSuffix` "idx"
  let (_, b') = runWriter $ do
        e <- readBindingOrCombinator b
        tell [IR.ArrayAssign vn (IR.Var idxName) e]

  tell [IR.LoopNTimes idxName lenExpr b']
  pure (IR.Var vn)

tryFigureOutTypeOf :: Maybe IR.Expr -> BindingOrCombinator -> IR.Type
tryFigureOutTypeOf lengthExpr (NamedBinding (Binding _ t)) = typeToIR lengthExpr t
tryFigureOutTypeOf lengthExpr (GroupBinding bs) = bindingListToIRType lengthExpr bs
tryFigureOutTypeOf lengthExpr (UnnamedBinding c) = combinatorToIRType lengthExpr c

typeToIR :: Maybe IR.Expr -> Type -> IR.Type
typeToIR _ (TerminalType t) = IR.TerminalType $ terminalTypeToIR t
typeToIR lengthExpr (CombinatorType c) = combinatorToIRType lengthExpr c

combinatorToIRType :: Maybe IR.Expr -> Combinator -> IR.Type
combinatorToIRType lengthExpr (NamedCombinator _ c) = typeToIR lengthExpr c
combinatorToIRType _ (ArrayCombinator len b) = IR.ArrayType (IR.ConstInt len) (tryFigureOutTypeOf (Just (IR.ConstInt len)) b)
-- combinatorToIRType le (GroupCombinator bs) = bindingListToIRType le bs
combinatorToIRType le (SepByCombinator _ bs) = bindingListToIRType le bs
combinatorToIRType lengthExpr (ListCombinator b) = IR.DynamicArrayType (tryFigureOutTypeOf lengthExpr b)

bindingListToIRType :: Maybe IR.Expr -> BindingList -> IR.Type
bindingListToIRType lengthExpr bindings@(BindingList bs) = IR.TupleOrStructType (generateStructName bindings) (fmap (\(Binding n t) -> (identToVarName n, typeToIR lengthExpr t)) bs)

terminalTypeToIR :: TerminalType -> IR.Terminal
terminalTypeToIR StringType = IR.StringTerminal
terminalTypeToIR IntType = IR.IntegerTerminal
terminalTypeToIR FloatType = IR.FloatTerminal

terminalVarToIR :: Ident -> TerminalType -> IRWriter IR.Expr
terminalVarToIR name StringType = do
  tell
    [ IR.DeclareVar (identToVarName name) (IR.TerminalType IR.StringTerminal)
    , IR.ReadVar (identToVarName name) IR.StringTerminal
    ]
  pure (IR.Var (identToVarName name))
terminalVarToIR name IntType = do
  tell
    [ IR.DeclareVar (identToVarName name) (IR.TerminalType IR.IntegerTerminal)
    , IR.ReadVar (identToVarName name) IR.IntegerTerminal
    ]
  pure (IR.Var (identToVarName name))
terminalVarToIR name FloatType = do
  tell
    [ IR.DeclareVar (identToVarName name) (IR.TerminalType IR.FloatTerminal)
    , IR.ReadVar (identToVarName name) IR.FloatTerminal
    ]
  pure (IR.Var (identToVarName name))

identToVarName :: Ident -> VarName
identToVarName (Ident name) = VarName (name :| [])
