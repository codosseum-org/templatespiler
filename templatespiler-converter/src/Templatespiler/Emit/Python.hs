module Templatespiler.Emit.Python where

import Prettyprinter
import Templatespiler.ToLang.Python

emitPy :: Program -> Doc ()
emitPy = vsep . fmap emitStmt

emitStmt :: Stmt -> Doc ()
emitStmt (Assign var e) = pretty var <+> "=" <+> emitExpr e
emitStmt (For i (Int 0) end stmts) =
  vsep
    [ "for" <+> pretty i <+> "in range(" <> emitExpr end <> "):"
    , indent 2 $ vsep $ fmap emitStmt stmts
    ]
emitStmt (For i start end stmts) =
  vsep
    [ "for" <+> (pretty i <+> "in range(" <> emitExpr start <> ", " <> emitExpr end) <> "):"
    , indent 2 $ vsep $ fmap emitStmt stmts
    ]
emitStmt (MultiAssign names e) = parens (hsep $ punctuate "," (fmap pretty names)) <+> "=" <+> emitExpr e
emitStmt (Append to e) = emitExpr to <> ".append(" <> emitExpr e <> ")"

emitExpr :: Expr -> Doc ()
emitExpr (Int i) = pretty i
emitExpr (Float f) = pretty f
emitExpr (String s) = toStringLit s
emitExpr None = "None"
emitExpr (Var v) = pretty v
emitExpr (List es) = list $ fmap emitExpr es
emitExpr Input = "input()"
emitExpr (CastToInt e) = "int" <> parens (emitExpr e)
emitExpr (CastToFloat e) = "float" <> parens (emitExpr e)
emitExpr (Split sep i) = emitExpr i <> ".split(" <> toStringLit sep <> ")"
emitExpr (Tuple es) = tupled $ fmap emitExpr es
emitExpr other = show other

toStringLit :: Text -> Doc ()
toStringLit t = dquotes $ pretty t
