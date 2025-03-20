module Templatespiler.Emit.Haskell where

import Language.Templatespiler.Pretty (prettyCombinator)
import Prettyprinter
import Templatespiler.Emit.Common (ConvertResult (..), PDoc, indentDepth)
import Templatespiler.ToLang.Haskell

haskellPreamble :: PDoc
haskellPreamble =
  vsep
    ["import Control.Monad"]

emitHaskellResult :: Either ToHaskellError (HaskellProgram, [ToHaskellWarning]) -> ConvertResult
emitHaskellResult (Left err) =
  ConversionFailed
    (emitToHaskellError err)
emitHaskellResult (Right (program, warnings)) =
  ConvertResult
    (emitHaskellWarning <$> warnings)
    (haskellPreamble <> line <> "main = " <> emitHaskell program)

emitHaskellWarning :: ToHaskellWarning -> PDoc
emitHaskellWarning x = case x of {}

emitToHaskellError :: ToHaskellError -> PDoc
emitToHaskellError (CantEmitSepByCombinator combinator sep _) =
  "Cannot emit the combinator" <+> prettyCombinator combinator <+> "as a Haskell expression, because the separator " <> dquotes (pretty sep) <> "is not a valid Haskell expression. Only single spaces are allowed as separators in Haskell."

emitHaskell :: Expression -> PDoc
emitHaskell = go False
  where
    go :: Bool -> Expression -> PDoc
    go _ (Int i) = pretty i
    go _ (Float f) = pretty f
    go _ (String s) = dquotes $ pretty s
    go _ (Var x) = pretty x
    go _ (Tuple xs) = tupled $ fmap (go False) xs
    go _ (List xs) = list $ fmap (go False) xs
    go _ (ReadLn t) = "readLn" <> maybe mempty (\inputType -> " @" <> pretty inputType) t
    go _ GetLine = "getLine"
    go _ (ReplicateM n x@(Do _)) = "replicateM " <> go False n <+> "$" <+> go False x
    go _ (ReplicateM n r@(ReadLn {})) = "replicateM" <+> go False n <> parens (go False r)
    go _ (ReplicateM n r@(Read {})) = "replicateM" <+> go False n <> parens (go False r)
    go _ (ReplicateM n x) = "replicateM" <+> go False n <+> go True x
    go _ (LetIn x e1 e2) = "let" <+> pretty x <+> "=" <+> go False e1 <+> "in" <+> go False e2
    go _ (Do xs) = emitDo xs
    go p (e1 :<$>: e2@(_ :<$>: _)) = maybeParens p $ go False e1 <+> "<$>" <+> parens (go False e2)
    go p (e1 :<$>: e2) = maybeParens p $ go False e1 <+> "<$>" <+> go True e2
    go _ (Read t e) = "read" <> maybe mempty (\inputType -> " @" <> pretty inputType) t <+> go True e
    go _ (Pure e) = "pure" <+> go True e
    go p (FmapCurried e) = maybeParens p $ "fmap" <+> go True e

    maybeParens :: Bool -> PDoc -> PDoc
    maybeParens True = parens
    maybeParens False = id

-- emitDo :: [Statement] -> PDoc
emitDo :: (Foldable t, Functor t) => t Statement -> PDoc
emitDo body =
  vsep
    [ "do"
    , indent indentDepth $ vsep $ toList $ fmap emitStatement body
    ]

emitStatement :: Statement -> PDoc
emitStatement (LiftExpr e) = emitHaskell e
emitStatement (Bind x e) = emitBinder x <+> "<-" <+> emitHaskell e
emitStatement (Let x e) = "let" <+> pretty x <+> "=" <+> emitHaskell e

emitBinder :: Binder -> PDoc
emitBinder (NamedBinder x) = pretty x
emitBinder (ListBinder xs) = list $ fmap emitBinder xs
emitBinder (TupleBinder xs) = tupled $ fmap emitBinder xs
