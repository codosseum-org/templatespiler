{-# LANGUAGE DerivingStrategies #-}

module Templatespiler.IR.Common where

import Data.Char (toUpper)
import Data.List.NonEmpty ((<|))
import Data.Text qualified as T
import Language.Templatespiler.Syntax
import Prettyprinter

{- | We represent var names as a non empty list of strings. This allows more idiomatic name generation - for example, we can turn
 @["orders", "len"]@ into either @ordersLen@ or @orders_len@ (or something else) depending on the target language
-}
newtype VarName
  = VarName
      (NonEmpty Text)
  deriving stock (Show, Eq)

instance IsString VarName where
  fromString = VarName . pure . fromString

withSuffix :: VarName -> Text -> VarName
withSuffix (VarName (n :| ns)) suffix = VarName (n :| (ns <> [suffix]))

data CaseStyle
  = CamelCase
  | PascalCase
  | SnakeCase
  | KebabCase

toCaseStyle :: CaseStyle -> VarName -> Text
toCaseStyle CamelCase (VarName (n :| ns)) =
  n
    <> mconcat (fmap (\n' -> toUpper (T.head n') `T.cons` T.tail n') ns)
toCaseStyle PascalCase (VarName (n :| ns)) =
  sconcat
    (fmap (\n' -> toUpper (T.head n') `T.cons` T.tail n') (n :| ns))
toCaseStyle SnakeCase (VarName (n :| ns)) = T.intercalate "_" (n : ns)
toCaseStyle KebabCase (VarName (n :| ns)) = T.intercalate "-" (n : ns)

instance Pretty VarName where
  pretty (VarName (n :| ns)) = pretty n <> hcat (fmap (("_" <>) . pretty) ns)

generateStructName :: BindingList -> VarName
generateStructName (BindingList bs) =
  VarName $ "Struct" <| fmap (\(Binding (Ident n) _) -> n) bs
