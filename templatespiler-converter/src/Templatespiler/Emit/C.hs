module Templatespiler.Emit.C where

import Prettyprinter
import Templatespiler.Emit.Common (ConvertResult (..), PDoc, indentDepth)
import Templatespiler.ToLang.C

emitToCError :: ToCError -> PDoc
emitToCError (LoopEndNotConvertible e) = "Loop end not convertible: " <+> pretty e

emitCWarning :: ToCWarning -> PDoc
emitCWarning (CantEmitCompoundType x) = "Can't emit compound type: " <+> pretty x

emitCResult :: Either ToCError (Program, [ToCWarning]) -> ConvertResult
emitCResult (Left e) = ConversionFailed $ emitToCError e
emitCResult (Right (program, warnings)) = ConvertResult (fmap emitCWarning warnings) (emitC program)

emitC :: Program -> PDoc
emitC program = do
  let inner = vsep $ fmap emitStmt program
  vsep
    [ "#include <stdio.h>"
    , ""
    , "int main() {"
    , indent indentDepth inner
    , "}"
    ]

emitStmt :: Stmt -> PDoc
emitStmt (Declare var t) = emitCTypePrefix t <+> pretty var <> emitCTypeSuffix t <> ";"
emitStmt (Assign var e) = pretty var <+> "=" <+> emitExpr e <> ";"
emitStmt (Scanf var vars) = "scanf(" <> dquotes (pretty var) <> ", " <> hsep (punctuate "," (fmap emitExpr vars)) <> ");"
emitStmt (Gets var) = "gets(" <> emitExpr var <> ");"
emitStmt (For i start end statements) =
  vsep
    [ "for (int" <+> pretty i <+> "=" <+> emitExpr start <+> ";" <+> pretty i <+> "<" <+> emitExpr end <+> ";" <+> pretty i <> "++) {"
    , indent indentDepth $ vsep $ fmap emitStmt statements
    , "}"
    , "" -- add a newline after the for loop for more readability
    ]
emitStmt (ListAssign to idx val) = emitExpr to <> brackets (emitExpr idx) <+> "=" <+> emitExpr val <> ";"

-- emitStmt (Typedef structName fields) =
--   "typedef"
--     <+> vsep
--       [ "struct {"
--       , indent indentDepth $ vsep $ fmap emitField fields
--       , "}"
--       ]
--     <+> pretty structName
--     <> ";"
--   where
--     emitField (name, t) = emitCTypePrefix t <+> pretty name <> emitCTypeSuffix t <> ";"

emitCTypePrefix :: CType -> PDoc
emitCTypePrefix IntType = "int"
emitCTypePrefix FloatType = "float"
emitCTypePrefix StringType = "char"
emitCTypePrefix (PointerType t) = emitCTypePrefix t <> "*"
emitCTypePrefix (ArrayType t _) = emitCTypePrefix t
emitCTypePrefix (StructType name) = pretty name

emitCTypeSuffix :: CType -> PDoc
emitCTypeSuffix (ArrayType _ size) = "[" <> emitExpr size <> "]"
emitCTypeSuffix StringType = "[500]"
emitCTypeSuffix _ = ""

emitExpr :: Expr -> PDoc
emitExpr (Int i) = pretty i
emitExpr (Float f) = pretty f
emitExpr (String s) = toStringLit s
emitExpr (Var v) = pretty v
emitExpr (Pointer e) = "&" <> emitExpr e
emitExpr (Deref e) = "*" <> emitExpr e
-- emitExpr (Struct name es) = parens (pretty name) <> braces (hsep $ punctuate "," (fmap emitExpr es))
emitExpr (Times e1 e2) = emitExpr e1 <+> "*" <+> emitExpr e2
emitExpr (Range start end) = "range(" <> emitExpr start <> ", " <> emitExpr end <> ")"

toStringLit :: Text -> PDoc
toStringLit t = dquotes $ pretty t
