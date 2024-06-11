module Templatespiler.ToLang.C where

import Data.Text qualified as Text
import Data.Traversable (for)
import Relude.Extra (bimapF)
import Templatespiler.IR.Common (CaseStyle (SnakeCase), VarName, toCaseStyle)
import Templatespiler.IR.Imperative qualified as IR
import Templatespiler.ToLang.Monad

data ToCWarning
type CWriterT m a = ToLangT [ToCWarning] m a
type CWriter a = CWriterT Identity a

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
  | Struct [Expr]
  deriving stock (Show)

data CType
  = IntType
  | FloatType
  | StringType
  | PointerType CType
  | ArrayType CType Expr
  | StructType [(Text, CType)]
  deriving stock (Show)

data Stmt
  = Assign Text Expr
  | Declare Text CType
  | For
      -- | integer for loop
      -- |  i in
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

toC :: IR.Program -> (Program, [ToCWarning])
toC = runToLang . fmap join . traverse statementToC . compress

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
  _ -> Scanf (formatSpecifier t) [var]

formatSpecifier :: IR.Terminal -> Text
formatSpecifier IR.IntegerTerminal = "%d"
formatSpecifier IR.FloatTerminal = "%f"
formatSpecifier IR.StringTerminal = "%s"

statementToC :: IR.Statement -> CWriter [Stmt]
statementToC (IR.DeclareVar name t) =
  pure
    [Declare (nameToCName name) (typeToCType t)]
statementToC (IR.ReadVar name t) = do
  pure
    [ Declare (nameToCName name) (terminalToCType t)
    , readTerminal t (Var $ nameToCName name)
    ]
statementToC (IR.ReadVars sep bindings) = do
  let scanfSpecifier = Text.intercalate sep (fmap (formatSpecifier . snd) (toList bindings))
  let split = Scanf scanfSpecifier (fmap (Var . nameToCName . fst) (toList bindings))
  let decls = fmap (\(name, t) -> Declare (nameToCName name) (terminalToCType t)) (toList bindings)

  pure (decls ++ [split])
statementToC (IR.LoopNTimes name end body) = do
  body' <- join <$> traverse statementToC body
  let end' = exprToC end
  pure [For (nameToCName name) (Int 0) end' body']
statementToC (IR.ArrayAssign name idx val) = do
  let idx' = exprToC idx
  let val' = exprToC val
  pure [ListAssign (Var (nameToCName name)) idx' val']

exprToC :: IR.Expr -> Expr
exprToC (IR.ConstInt i) = Int i
exprToC (IR.Var vn) = Var (nameToCName vn)
exprToC (IR.TupleOrStruct _ es) = Struct $ fmap exprToC (toList es)

terminalToCType :: IR.Terminal -> CType
terminalToCType IR.StringTerminal = StringType
terminalToCType IR.IntegerTerminal = IntType
terminalToCType IR.FloatTerminal = FloatType

typeToCType :: IR.Type -> CType
typeToCType (IR.TerminalType t) = terminalToCType t
typeToCType (IR.ArrayType len t) = ArrayType (typeToCType t) (exprToC len)
typeToCType (IR.TupleOrStructType _ ts) = StructType $ bimapF nameToCName typeToCType (toList ts)
typeToCType (IR.DynamicArrayType t) = PointerType (typeToCType t)

nameToCName :: VarName -> Text
nameToCName = toCaseStyle SnakeCase
