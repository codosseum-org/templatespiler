-- | Imperative flavoured IR for templatespiler.
module Templatespiler.IR.Imperative where

{- | We represent var names as a non empty list of strings. This allows more idiomatic name generation - for example, we can turn
 @["orders", "len"]@ into either @ordersLen@ or @orders_len@ (or something else) depending on the target language
-}
newtype VarName
  = VarName
      (NonEmpty Text)
  deriving (Show)

withSuffix :: VarName -> Text -> VarName
withSuffix (VarName (n :| ns)) suffix = VarName (n :| (ns <> [suffix]))

data Statement
  = -- | Variable declaration. This may be a C-style declaration (@int x@) or an assignment (@x = 0@) depending on the target language.
    Decl
      VarName
      -- ^ The variable name
      VarType
      -- ^ The type
  | Assign VarName VarType Expr
  | MultiReadAssign
      Text -- Separator
      (NonEmpty (VarName, ReadType))
  | For
      VarName -- variable name
      Expr -- start
      Expr -- end
      [Statement] -- body
  | AppendToArray
      VarName
      -- ^ array name
      Expr
      -- ^ index
      Expr
      -- ^ value
  deriving (Show)

data VarType
  = IntType
  | FloatType
  | StringType
  | ArrayType Expr VarType
  | UnknownType
  deriving (Show)

data Expr
  = ConstInt Int
  | Var VarName
  | ReadAtom ReadType
  | TupleOrStruct (Maybe VarName) (NonEmpty Expr)
  deriving (Show)

data ReadType
  = ReadInt
  | ReadFloat
  | ReadString
  deriving (Show)
