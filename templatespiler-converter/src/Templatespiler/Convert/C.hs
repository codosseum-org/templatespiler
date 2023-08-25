module Templatespiler.Convert.C where

import Control.Monad (foldM)
import Data.Char (toUpper)
import Data.Text qualified as T
import Prettyprinter (Pretty (pretty), encloseSep, line')
import Templatespiler.Convert.Common
import Templatespiler.IR.Imperative as IR

convertToC :: [Statement] -> DocBuilder ()
convertToC = foldM (\_ b -> writeBinding b) ()

prettyType :: VarType -> FallibleDocBuilder ()
prettyType StringType = write "char*"
prettyType IntType = write "int"
prettyType FloatType = write "double"
prettyType UnknownType = do
  warn (CantConvertType UnknownType C)
  fail ""
prettyType (ArrayType len t) = do
  y <- prettyType t
  write "["
  write =<< prettyExpr len
  write "]"
  pure y

eol :: (MonadDocBuilder m) => m ()
eol = write ";" *> write line'

declareVar :: (MonadDocBuilder m) => VarName -> VarType -> m ()
declareVar name t = fromFallible $ do
  prettyType t
  write " "
  write $ prettyVarName name
  eol

scanf :: (MonadDocBuilder m) => Text -> [(ReadType, VarName)] -> m ()
scanf separator vars = do
  write "scanf(\""
  let scanfText = T.intercalate separator $ printfDirective . fst <$> vars
  write $ pretty scanfText
  write "\\n\", "
  let varNames = prettyVarName . snd <$> vars
  write $ encloseSep "" "" ", " varNames
  write ")"
  write ";"

writeBinding :: Statement -> DocBuilder ()
writeBinding (Decl name t) = declareVar name t
writeBinding (Assign name t val) = fromFallible $ do
  prettyType t
  write " "
  write $ prettyVarName name
  case val of
    ReadAtom a -> do
      eol
      scanf " " [(a, name)]
    _ -> do
      val' <- prettyExpr val
      write " = "
      write val'
      eol
writeBinding (For name start end body) = fromFallible $ do
  write "for (int "
  write $ prettyVarName name
  write " = "
  write =<< prettyExpr start
  write "; "
  write $ prettyVarName name
  write " < "
  write =<< prettyExpr end
  write "; "
  write $ prettyVarName name
  write "++)"
  write line'

  y <- mapM (toFallible . listenOnly' . writeBinding) body
  toFallible $ curlyBlock y
  write line'
writeBinding (MultiReadAssign sep parts) = do
  -- Firstly declare all variables
  traverse_ (\(name, t) -> declareVar name (readTypeToVarType t)) parts
  scanf sep (swap <$> toList parts)
writeBinding (AppendToArray name idx val) = fromFallible $ do
  write $ prettyVarName name
  write "["
  write =<< prettyExpr idx
  write "] = "
  write =<< prettyExpr val
  write ";"

prettyExpr :: Expr -> FallibleDocBuilder Doc'
prettyExpr (ConstInt i) = pure $ pretty i
prettyExpr (Var name) = pure $ prettyVarName name
prettyExpr (ReadAtom rt) = do
  pure $ prettyReadAtom "input()" rt
prettyExpr e@(TupleOrStruct _ _) = do
  warn $ CantConvertExpr e C
  fail ""

printfDirective :: ReadType -> Text
printfDirective ReadInt = "%d"
printfDirective ReadFloat = "%f"
printfDirective ReadString = "%s"

toPyStrLit :: Text -> Text
toPyStrLit s = "'" <> s <> "'"

prettyReadAtom :: Doc' -> ReadType -> Doc'
prettyReadAtom source ReadInt = "int(" <> source <> ")"
prettyReadAtom source ReadFloat = "float(" <> source <> ")"
prettyReadAtom source ReadString = source

prettyVarName :: VarName -> Doc'
prettyVarName (VarName parts) = prettyVarName' (toList parts)
  where
    prettyVarName' :: [Text] -> Doc'
    prettyVarName' [] = mempty
    prettyVarName' [n] = pretty n
    prettyVarName' (n : ns) = pretty n <> prettyVarName' (upperFirst ns)

    upperFirst :: [Text] -> [Text]
    upperFirst [] = []
    upperFirst (n : ns) = (toUpper (T.head n) `T.cons` T.tail n) : ns
