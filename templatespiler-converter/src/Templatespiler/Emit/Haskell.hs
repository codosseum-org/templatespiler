module Templatespiler.Emit.Haskell where

import Prettyprinter
import Templatespiler.Emit.Common (ConvertResult (ConvertResult), PDoc, indentDepth)
import Templatespiler.ToLang.Haskell

haskellPreamble :: PDoc
haskellPreamble =
  vsep
    ["import Control.Monad"]

emitHaskellResult :: (HaskellProgram, [ToHaskellWarning]) -> ConvertResult
emitHaskellResult (program, warnings) =
  ConvertResult
    (emitHaskellWarning <$> warnings)
    (haskellPreamble <> line <> "main = " <> emitHaskell program)

emitHaskellWarning :: ToHaskellWarning -> PDoc
emitHaskellWarning x = case x of {}

emitHaskell :: Expression -> PDoc
emitHaskell (Int i) = pretty i
emitHaskell (Float f) = pretty f
emitHaskell (String s) = dquotes $ pretty s
emitHaskell (Var x) = pretty x
emitHaskell (Tuple xs) = tupled $ fmap emitHaskell xs
emitHaskell (List xs) = list $ fmap emitHaskell xs
emitHaskell (ReadLn t) = "readLn" <> maybe mempty (\inputType -> " @" <> pretty inputType) t
emitHaskell GetLine = "getLine"
emitHaskell (ReplicateM n x@(Do _)) = "replicateM " <> emitHaskell n <+> "$" <+> emitHaskell x
emitHaskell (ReplicateM n x) = "replicateM" <+> emitHaskell n <+> emitHaskell x
emitHaskell (LetIn x e1 e2) = "let" <+> pretty x <+> "=" <+> emitHaskell e1 <+> "in" <+> emitHaskell e2
emitHaskell (Do xs) = emitDo xs
emitHaskell (e1 :<$>: (e2 :<$>: e3)) = emitHaskell e1 <+> "<$>" <+> emitHaskell e2 <+> "<$>" <+> emitHaskell e3
emitHaskell (e1 :<$>: e2) = emitHaskell e1 <+> "<$>" <+> emitHaskell e2
emitHaskell (Read t e) = "read" <> maybe mempty (\inputType -> " @" <> pretty inputType) t <+> emitHaskell e
emitHaskell (Pure e) = "pure" <+> emitHaskell e

-- emitDo :: [Statement] -> PDoc
emitDo :: (Foldable t, Functor t) => t Statement -> PDoc
emitDo body =
  vsep
    [ "do"
    , indent indentDepth $ vsep $ toList $ fmap emitStatement body
    , "" -- add a newline after the do block for more readability
    ]

emitStatement :: Statement -> PDoc
emitStatement (LiftExpr e) = emitHaskell e
emitStatement (Bind x e) = emitBinder x <+> "<-" <+> emitHaskell e
emitStatement (Let x e) = "let" <+> pretty x <+> "=" <+> emitHaskell e

emitBinder :: Binder -> PDoc
emitBinder (NamedBinder x) = pretty x
emitBinder (ListBinder xs) = list $ fmap emitBinder xs
emitBinder (TupleBinder xs) = tupled $ fmap emitBinder xs
