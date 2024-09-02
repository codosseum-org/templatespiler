{-# LANGUAGE PartialTypeSignatures #-}

module Templatespiler.ToLang.C where

import Control.Monad.Except (throwError)
import Control.Monad.Writer (tell)
import Data.Text qualified as Text
import Templatespiler.IR.Common (CaseStyle (PascalCase, SnakeCase), VarName, toCaseStyle)
import Templatespiler.IR.Imperative qualified as IR
import Templatespiler.ToLang.Monad

newtype ToCWarning
  = CantEmitCompoundType IR.Type

newtype ToCError
  = LoopEndNotConvertible IR.Expr
  deriving stock (Show)
type CWriterT m a = ToLangT [ToCWarning] m a
type CWriter a = CWriterT (ExceptT ToCError (State TypedefState)) a

data TypedefState = TypedefState
  {
  }
  deriving stock (Show)

-- | Very stripped down version of the C AST.
data Expr
  = Int Int
  | Float Float
  | String Text
  | Var Text
  | Times Expr Expr
  | Range Expr Expr
  | Pointer Expr
  | Deref Expr
  deriving stock (Show)

data CType
  = IntType
  | FloatType
  | StringType
  | PointerType CType
  | ArrayType CType Expr
  | StructType Text
  deriving stock (Show)

data Stmt
  = Assign Text Expr
  | Declare Text CType
  | For
      -- | integer for loop
      -- | i in
      Text
      -- | start
      Expr
      -- | end
      Expr
      -- | body
      [Stmt]
  | ListAssign Expr Expr Expr
  | Scanf Text [Expr]
  | Gets Expr
  deriving stock (Show)

type Program = [Stmt]

toC :: IR.Program -> Either ToCError (Program, [ToCWarning])
toC x = do
  fst <$> usingState TypedefState $ runExceptT $ runToLangT $ fmap join $ traverse statementToC $ compress x

compress :: [IR.Statement] -> [IR.Statement]
compress [] = []
-- because C doesn't need variable declarations, we can compress them away
compress (IR.DeclareVar vn1 (IR.TerminalType t1) : IR.ReadVar vn2 t2 : xs)
  | vn1 == vn2 && t1 == t2 =
      IR.ReadVar vn1 t1 : compress xs
compress (IR.LoopNTimes v n b : xs) = IR.LoopNTimes v n (compress b) : compress xs
compress (x : xs) = x : compress xs

readTerminal :: IR.Terminal -> Expr -> Stmt
readTerminal t var = case t of
  IR.StringTerminal -> Gets var
  _ -> Scanf (formatSpecifier t) [Pointer var]

formatSpecifier :: IR.Terminal -> Text
formatSpecifier IR.IntegerTerminal = "%d"
formatSpecifier IR.FloatTerminal = "%f"
formatSpecifier IR.StringTerminal = "%s"

statementToC :: IR.Statement -> CWriter [Stmt]
statementToC (IR.DeclareVar name t) =
  do
    t' <- typeToCType t
    case t' of
      Nothing -> pure []
      Just t'' -> pure [Declare (nameToCName name) t'']
statementToC (IR.ReadVar name t) = do
  pure
    [ Declare (nameToCName name) (terminalToCType t)
    , readTerminal t (Var $ nameToCName name)
    ]
statementToC (IR.ReadVars sep bindings) = do
  let scanfSpecifier = Text.intercalate sep (fmap (formatSpecifier . snd) (toList bindings))
  let scanfAddPointerIfNecessary :: VarName -> IR.Terminal -> Expr
      scanfAddPointerIfNecessary v IR.StringTerminal = Var $ nameToCName v
      scanfAddPointerIfNecessary v _ = Pointer $ Var $ nameToCName v
  let split = Scanf scanfSpecifier (fmap (uncurry scanfAddPointerIfNecessary) (toList bindings))
  let decls = fmap (\(name, t) -> Declare (nameToCName name) (terminalToCType t)) (toList bindings)

  pure (decls ++ [split])
statementToC (IR.LoopNTimes name end body) = do
  body' <- join <$> traverse statementToC body
  end' <- exprToC end
  case end' of
    Nothing -> throwError $ LoopEndNotConvertible end
    Just end'' -> pure [For (nameToCName name) (Int 0) end'' body']
statementToC (IR.ArrayAssign name idx val) = do
  idx' <- exprToC idx
  val' <- exprToC val
  case (idx', val') of
    (Just idx'', Just val'') ->
      pure [ListAssign (Var (nameToCName name)) idx'' val'']
    _ -> pure []

exprToC :: IR.Expr -> CWriter (Maybe Expr)
exprToC (IR.ConstInt i) = pure $ Just $ Int i
exprToC (IR.Var vn) = pure $ Just $ Var (nameToCName vn)
exprToC (IR.TupleOrStruct name es) = do
  pure Nothing

-- Struct (nameToCStructName name) <$> traverse exprToC (toList es)

terminalToCType :: IR.Terminal -> CType
terminalToCType IR.StringTerminal = StringType
terminalToCType IR.IntegerTerminal = IntType
terminalToCType IR.FloatTerminal = FloatType

typeToCType :: IR.Type -> CWriter (Maybe CType)
typeToCType x = runMaybeT $ typeToCType' x
  where
    typeToCType' (IR.TerminalType t) = pure $ terminalToCType t
    typeToCType' (IR.ArrayType len t) = do
      t' <- typeToCType' t
      e' <- hoistMaybe <$> lift (exprToC len)
      ArrayType t' <$> e'
    typeToCType' (IR.TupleOrStructType name ts) = do
      lift $ tell [CantEmitCompoundType (IR.TupleOrStructType name ts)]
      hoistMaybe Nothing
    -- case Map.lookup structName defs of
    --   Just (NewTypedef t _) -> pure $ StructType t
    --   Nothing -> do
    --     ts' <- traverse (\(n, t) -> (,) (nameToCName n) <$> typeToCType t) (toList ts)
    --     let t = StructType structName
    --     State.modify' $ \s -> s {typeDefs = Map.insert structName (NewTypedef structName ts') (typeDefs s)}
    --     pure t
    typeToCType' (IR.DynamicArrayType t) = do
      t' <- typeToCType' t
      pure $ PointerType t'

nameToCName :: VarName -> Text
nameToCName = toCaseStyle SnakeCase

nameToCStructName :: VarName -> Text
nameToCStructName = toCaseStyle PascalCase
