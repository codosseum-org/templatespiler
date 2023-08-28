{-# LANGUAGE OverloadedStrings #-}

module Language.Templatespiler.Pretty where

import Language.Templatespiler.Syntax (Binding (..), BindingList (..), Combinator (..), Ident (..), TerminalType (..), Type (..))
import Prettyprinter
import Prettyprinter.Render.Terminal
import Prelude hiding (Type, group)

prettyBindingList :: BindingList -> Doc AnsiStyle
prettyBindingList (BindingList bindings) =
  vsep $ toList $ fmap prettyBinding bindings

prettyBinding :: Binding -> Doc AnsiStyle
prettyBinding (Binding name typ) =
  prettyIdent name <+> colon <+> prettyType typ

prettyType :: Type -> Doc AnsiStyle
prettyType (TerminalType t) = prettyTerminalType t
prettyType c = prettyType1 c

prettyType1 :: Type -> Doc AnsiStyle
prettyType1 (CombinatorType c) = prettyCombinator1 c
prettyType1 c = parens (prettyType c)

prettyCombinator :: Combinator -> Doc AnsiStyle
prettyCombinator (SepByCombinator sep typ) =
  "sep-by" <+> dquotes (pretty sep) <+> prettyType typ
prettyCombinator (NamedCombinator name typ) =
  prettyIdent name <+> colon <+> prettyType typ
prettyCombinator c = prettyCombinator1 c

prettyCombinator1 :: Combinator -> Doc AnsiStyle
prettyCombinator1 (GroupCombinator (BindingList bindings)) =
  group $
    vsep
      [ nest 2 $
          vsep
            [ lbracket
            , vsep $ toList $ fmap prettyBinding bindings
            ]
      , rbracket
      ]
prettyCombinator1 (ArrayCombinator len typ) =
  "array" <+> pretty len <+> prettyType typ
prettyCombinator1 (ListCombinator typ) =
  "list" <+> prettyType typ
prettyCombinator1 c = parens (prettyCombinator c)

prettyTerminalType :: TerminalType -> Doc AnsiStyle
prettyTerminalType IntType = "Integer"
prettyTerminalType FloatType = "Float"
prettyTerminalType StringType = "String"

prettyIdent :: Ident -> Doc AnsiStyle
prettyIdent (Ident name) = pretty name
