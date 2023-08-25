-- | Imperative flavoured IR for templatespiler.
module Templatespiler.IR.Imperative where

{- | We represent var names as a non empty list of strings. This allows more idiomatic name generation - for example, we can turn
 @["orders", "len"]@ into either @ordersLen@ or @orders_len@ (or something else) depending on the target language
-}
newtype VarName
  = VarName
      (NonEmpty Text)

withSuffix :: VarName -> Text -> VarName
withSuffix (VarName (n :| ns)) suffix = VarName (n :| (ns <> [suffix]))

data Statement
  = Decl VarName Expr
  | For
      VarName -- variable name
      Expr -- start
      Expr -- end
      [Statement] -- body

data Expr
  = ConstInt Int
  | Var VarName
  | Read VarName [ReadType]
  | TupleOrStruct (Maybe VarName) (NonEmpty Expr)

data ReadType
  = ReadInt
  | ReadFloat
  | ReadString
  | ReadConst Text
