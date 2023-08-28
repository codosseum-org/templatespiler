{-# LANGUAGE DerivingStrategies #-}

module Templatespiler.IR.Common where

import Prettyprinter

{- | We represent var names as a non empty list of strings. This allows more idiomatic name generation - for example, we can turn
 @["orders", "len"]@ into either @ordersLen@ or @orders_len@ (or something else) depending on the target language
-}
newtype VarName
  = VarName
      (NonEmpty Text)
  deriving stock (Show)

instance IsString VarName where
  fromString = VarName . pure . fromString

withSuffix :: VarName -> Text -> VarName
withSuffix (VarName (n :| ns)) suffix = VarName (n :| (ns <> [suffix]))

instance Pretty VarName where
  pretty (VarName (n :| ns)) = pretty n <> hcat (fmap (("_" <>) . pretty) ns)
