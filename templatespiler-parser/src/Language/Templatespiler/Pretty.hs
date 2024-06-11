{-# LANGUAGE OverloadedStrings #-}

module Language.Templatespiler.Pretty where

import Language.Templatespiler.Syntax (Binding (..), BindingList (..), BindingOrCombinator (..), Combinator (..), Ident (..), TerminalType (..), Type (..))
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
prettyCombinator (SepByCombinator sep (BindingList typ)) =
  "sep-by" <+> dquotes (pretty sep) <+> prettyGroupCombinator typ
prettyCombinator (NamedCombinator name typ) =
  prettyIdent name <+> colon <+> prettyType typ
prettyCombinator (GroupCombinator (BindingList typ)) = prettyGroupCombinator typ
prettyCombinator c = prettyCombinator1 c

prettyGroupCombinator :: (Foldable t, Functor t) => t Binding -> Doc AnsiStyle
prettyGroupCombinator bindings =
  group $
    vsep
      [ nest 2 $
          vsep
            [ lbracket
            , vsep $ toList $ fmap prettyBinding bindings
            ]
      , rbracket
      ]

prettyCombinator1 :: Combinator -> Doc AnsiStyle
prettyCombinator1 (ArrayCombinator len typ) =
  "array" <+> pretty len <+> prettyBindingOrCombinator typ
prettyCombinator1 (ListCombinator typ) =
  "list" <+> prettyBindingOrCombinator typ
prettyCombinator1 c = parens (prettyCombinator c)

prettyBindingOrCombinator :: BindingOrCombinator -> Doc AnsiStyle
prettyBindingOrCombinator (NamedBinding b) = prettyBinding b
prettyBindingOrCombinator (GroupBinding (BindingList bl)) = prettyGroupCombinator bl
prettyBindingOrCombinator (UnnamedBinding c) = prettyCombinator c

prettyTerminalType :: TerminalType -> Doc AnsiStyle
prettyTerminalType IntType = "Integer"
prettyTerminalType FloatType = "Float"
prettyTerminalType StringType = "String"

prettyIdent :: Ident -> Doc AnsiStyle
prettyIdent (Ident name) = pretty name
