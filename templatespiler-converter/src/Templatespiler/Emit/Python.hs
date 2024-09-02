module Templatespiler.Emit.Python where

import Prettyprinter
import Templatespiler.Emit.Common (ConvertResult (ConvertResult), PDoc, indentDepth)
import Templatespiler.ToLang.Python

emitPyWarning :: ToPythonWarning -> PDoc
emitPyWarning x = case x of {}

emitPyResult :: (Program, [ToPythonWarning]) -> ConvertResult
emitPyResult (program, warnings) = ConvertResult (emitPyWarning <$> warnings) (emitPy program)

emitPy :: Program -> Doc ()
emitPy = vsep . fmap emitStmt

emitFor :: (Pretty a) => a -> Doc () -> [Stmt] -> Doc ()
emitFor i rangeParams body =
  vsep
    [ "for" <+> pretty i <+> "in range(" <> rangeParams <> "):"
    , indent indentDepth $ vsep $ fmap emitStmt body
    , "" -- add a newline after the for loop for more readability
    ]

emitStmt :: Stmt -> Doc ()
emitStmt (Assign var e) = pretty var <+> "=" <+> emitExpr e
emitStmt (For i (Int 0) end statements) = emitFor i (emitExpr end) statements
emitStmt (For i start end statements) = emitFor i (emitExpr start <> ", " <> emitExpr end) statements
emitStmt (MultiAssign names e) = parens (hsep $ punctuate "," (fmap pretty names)) <+> "=" <+> emitExpr e
emitStmt (Append to e) = emitExpr to <> ".append(" <> emitExpr e <> ")"
emitStmt (ListAssign to idx val) = emitExpr to <> brackets (emitExpr idx) <+> "=" <+> emitExpr val

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
emitExpr (Split separator i) = emitExpr i <> ".split(" <> toStringLit separator <> ")"
emitExpr (Tuple es) = tupled $ fmap emitExpr es
emitExpr (Times e1 e2) = emitExpr e1 <+> "*" <+> emitExpr e2
emitExpr (Range start end) = "range(" <> emitExpr start <> ", " <> emitExpr end <> ")"
emitExpr (Map f x) = "map" <> parens (emitExpr f <> ", " <> emitExpr x)

toStringLit :: Text -> Doc ()
toStringLit t = dquotes $ pretty t
