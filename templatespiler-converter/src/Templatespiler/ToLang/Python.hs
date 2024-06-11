module Templatespiler.ToLang.Python where

import Templatespiler.IR.Common (CaseStyle (SnakeCase), VarName, toCaseStyle)
import Templatespiler.IR.Imperative qualified as IR
import Templatespiler.ToLang.Monad

data ToPythonWarning
type PythonWriterT m a = ToLangT [ToPythonWarning] m a
type PythonWriter a = PythonWriterT Identity a

-- | Very stripped down version of the Python AST.
data Expr
  = Int Int
  | Float Float
  | String Text
  | None
  | Var Text
  | Tuple [Expr]
  | List [Expr]
  | Range Expr Expr
  | Input
  | CastToInt Expr
  | CastToFloat Expr
  | Split Text Expr
  | -- | Call to python map() function
    Map
      Expr
      Expr
  deriving (Show)

data Stmt
  = Assign Text Expr
  | MultiAssign [Text] Expr
  | For
      Text
      -- ^ i in
      Expr
      -- ^ start
      Expr
      -- ^ end
      [Stmt]
      -- ^ body
  | Append Expr Expr
  deriving (Show)

type Program = [Stmt]

toPython :: IR.Program -> (Program, [ToPythonWarning])
toPython = runToLang . fmap join . traverse undefined

-- statementToPython :: IR.Statement -> PythonWriter [Stmt]
-- statementToPython (IR.Decl name t) =
--   pure
--     [Assign (nameToPyName name) (defaultTypeValue t)]
-- statementToPython (IR.Assign name _ e) = do
--   e' <- exprToPython e
--   pure
--     [ Assign
--         (nameToPyName name)
--         e'
--     ]
-- statementToPython (IR.MultiReadAssign sep bindings) = do
--   let i = Input
--   let split = Split sep i
--   let uniqBindingsTypes = ordNub (fmap snd (toList bindings))
--   case uniqBindingsTypes of
--     [] -> error "impossible"
--     [IR.ReadString] ->
--       -- if we're only reading strings, we already have all the parts as a list, so we just need to assign
--       pure [MultiAssign (fmap (nameToPyName . fst) (toList bindings)) split]
--     [IR.ReadInt] -> do
--       let mapped = Map (Var "int") split -- map all inputs to int
--       pure [MultiAssign (fmap (nameToPyName . fst) (toList bindings)) mapped]
--     [IR.ReadFloat] -> do
--       let mapped = Map (Var "float") split
--       pure [MultiAssign (fmap (nameToPyName . fst) (toList bindings)) mapped]
--     _ -> do
--       let multi = MultiAssign (fmap (nameToPyName . fst) (toList bindings)) split
--       let makeCast (name, as) = case as of
--             IR.ReadString -> Nothing
--             IR.ReadInt -> Just $ Assign (nameToPyName name) (CastToInt (Var (nameToPyName name)))
--             IR.ReadFloat -> Just $ Assign (nameToPyName name) (CastToFloat (Var (nameToPyName name)))
--       let casts = mapMaybe makeCast (toList bindings)
--       pure (multi : casts)
-- statementToPython (IR.For v start end body) = do
--   start' <- exprToPython start
--   end' <- exprToPython end
--   body' <- join <$> traverse statementToPython body
--   pure [For (nameToPyName v) start' end' body']
-- statementToPython (IR.AppendToArray arr _ val) = do
--   val' <- exprToPython val
--   pure [Append (Var $ nameToPyName arr) val']
-- exprToPython :: IR.Expr -> PythonWriter Expr
-- exprToPython (IR.ConstInt i) = pure (Int i)
-- exprToPython (IR.Var n) = pure (Var (nameToPyName n))
-- exprToPython (IR.ReadAtom IR.ReadString) = pure Input
-- exprToPython (IR.ReadAtom IR.ReadInt) = pure (CastToInt Input)
-- exprToPython (IR.ReadAtom IR.ReadFloat) = pure (CastToFloat Input)
-- exprToPython (IR.TupleOrStruct _ exprs) = do
--   exprs' <- traverse exprToPython exprs
--   pure (Tuple (toList exprs'))

-- defaultTypeValue :: IR.VarType -> Expr
-- defaultTypeValue IR.IntType = Int 0
-- defaultTypeValue IR.FloatType = Float 0.0
-- defaultTypeValue IR.StringType = String ""
-- defaultTypeValue (IR.ArrayType _ _) = List []
-- defaultTypeValue (IR.DynamicArrayType _) = List []
-- defaultTypeValue (IR.TupleOrStructType _ _) = Tuple []
-- defaultTypeValue IR.UnknownType = None

nameToPyName :: VarName -> Text
nameToPyName = toCaseStyle SnakeCase
