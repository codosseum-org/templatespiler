module Templatespiler.ToLang.Python where

import Data.Traversable (for)
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
  | Times Expr Expr
  | Range Expr Expr
  | Input
  | CastToInt Expr
  | CastToFloat Expr
  | Split Text Expr
  | -- | Call to python map() function
    Map
      Expr
      Expr
  deriving stock (Show)

data Stmt
  = Assign Text Expr
  | MultiAssign [Text] Expr
  | For
      -- | i in
      Text
      -- | start
      Expr
      -- | end
      Expr
      -- | body
      [Stmt]
  | Append Expr Expr
  | ListAssign Expr Expr Expr
  deriving stock (Show)

type Program = [Stmt]

toPython :: IR.Program -> (Program, [ToPythonWarning])
toPython = runToLang . fmap join . traverse statementToPython . compress

compress :: [IR.Statement] -> [IR.Statement]
compress [] = []
-- because python doesn't need variable declarations, we can compress them away
compress (IR.DeclareVar vn1 (IR.TerminalType t1) : IR.ReadVar vn2 t2 : xs)
  | vn1 == vn2 && t1 == t2 =
      IR.ReadVar vn1 t1 : compress xs
compress (IR.LoopNTimes v n b : xs) = IR.LoopNTimes v n (compress b) : compress xs
compress (x : xs) = x : compress xs

castTerminal :: IR.Terminal -> Expr -> Expr
castTerminal t input = case t of
  IR.IntegerTerminal -> CastToInt input
  IR.FloatTerminal -> CastToFloat input
  IR.StringTerminal -> input

statementToPython :: IR.Statement -> PythonWriter [Stmt]
statementToPython (IR.DeclareVar name t) =
  pure
    [Assign (nameToPyName name) (defaultTypeValue t)]
statementToPython (IR.ReadVar name t) = do
  let inputExpr = castTerminal t Input
  pure [Assign (nameToPyName name) inputExpr]
statementToPython (IR.ReadVars sep bindings) = do
  let split = Split sep Input
  let multi = MultiAssign (fmap (nameToPyName . fst) (toList bindings)) split
  let makeCast (name, as) = case as of
        IR.StringTerminal -> Nothing
        t -> Just (Assign (nameToPyName name) (castTerminal t $ Var $ nameToPyName name))

  let casts = mapMaybe makeCast (toList bindings)
  pure (multi : casts)
statementToPython (IR.LoopNTimes name end body) = do
  body' <- join <$> traverse statementToPython body
  let end' = exprToPython end
  pure [For (nameToPyName name) (Int 0) end' body']
statementToPython (IR.ArrayAssign name idx val) = do
  let idx' = exprToPython idx
  let val' = exprToPython val
  pure [ListAssign (Var (nameToPyName name)) idx' val']

exprToPython :: IR.Expr -> Expr
exprToPython (IR.ConstInt i) = Int i
exprToPython (IR.Var vn) = Var (nameToPyName vn)
exprToPython (IR.TupleOrStruct _ es) = Tuple $ fmap exprToPython (toList es)

defaultTypeValue :: IR.Type -> Expr
defaultTypeValue (IR.TerminalType IR.IntegerTerminal) = Int 0
defaultTypeValue (IR.TerminalType IR.FloatTerminal) = Float 0.0
defaultTypeValue (IR.TerminalType IR.StringTerminal) = String ""
defaultTypeValue (IR.ArrayType len _) = List [None] `Times` exprToPython len
defaultTypeValue (IR.DynamicArrayType _) = List []
defaultTypeValue (IR.TupleOrStructType _ _) = Tuple []

nameToPyName :: VarName -> Text
nameToPyName = toCaseStyle SnakeCase
