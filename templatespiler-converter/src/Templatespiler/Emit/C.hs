module Templatespiler.Emit.C where

import Prettyprinter
import Templatespiler.Emit.Common (indentDepth)
import Templatespiler.ToLang.C

emitC :: Program -> Doc ()
emitC program = do
  let inner = vsep $ fmap emitStmt program
  vsep
    [ "#include <stdio.h>"
    , ""
    , "int main() {"
    , indent indentDepth inner
    , "}"
    ]

emitStmt :: Stmt -> Doc ()
emitStmt (Declare var t) = emitCTypePrefix t <+> pretty var <> emitCTypeSuffix t <> ";"
emitStmt (Assign var e) = pretty var <+> "=" <+> emitExpr e <> ";"
emitStmt (Scanf var vars) = "scanf(" <> dquotes (pretty var) <> ", " <> hsep (punctuate ", " (fmap emitExpr vars)) <> ");"
emitStmt (Gets var) = "gets(" <> emitExpr var <> ");"
emitStmt (For i start end statements) =
  vsep
    [ "for (int" <+> pretty i <+> "=" <+> emitExpr start <+> ";" <+> pretty i <+> "<" <+> emitExpr end <+> ";" <+> pretty i <> "++) {"
    , indent indentDepth $ vsep $ fmap emitStmt statements
    , "}"
    , "" -- add a newline after the for loop for more readability
    ]
emitStmt (ListAssign to idx val) = emitExpr to <> brackets (emitExpr idx) <+> "=" <+> emitExpr val <> ";"

emitCTypePrefix :: CType -> Doc ()
emitCTypePrefix IntType = "int"
emitCTypePrefix FloatType = "float"
emitCTypePrefix StringType = "char*"
emitCTypePrefix (PointerType t) = emitCTypePrefix t <> "*"
emitCTypePrefix (ArrayType t _) = emitCTypePrefix t
emitCTypePrefix (StructType fields) =
  vsep
    [ "struct {"
    , indent indentDepth $ vsep $ fmap emitField fields
    , "}"
    ]
  where
    emitField (name, t) = emitCTypePrefix t <+> pretty name <> ";"

emitCTypeSuffix :: CType -> Doc ()
emitCTypeSuffix (ArrayType _ size) = "[" <> emitExpr size <> "]"
emitCTypeSuffix _ = ""

emitExpr :: Expr -> Doc ()
emitExpr (Int i) = pretty i
emitExpr (Float f) = pretty f
emitExpr (String s) = toStringLit s
emitExpr (Var v) = pretty v
emitExpr (Pointer e) = "&" <> emitExpr e
emitExpr (Deref e) = "*" <> emitExpr e
emitExpr (Struct es) = braces $ hsep $ punctuate "," (fmap emitExpr es)
emitExpr (Times e1 e2) = emitExpr e1 <+> "*" <+> emitExpr e2
emitExpr (Range start end) = "range(" <> emitExpr start <> ", " <> emitExpr end <> ")"

toStringLit :: Text -> Doc ()
toStringLit t = dquotes $ pretty t
