module Templatespiler.Convert.Python where

import Control.Monad (foldM)
import Data.List.NonEmpty as NE (zip)
import Prettyprinter (Pretty (pretty), tupled)
import Templatespiler.Convert.Common
import Templatespiler.IR.Imperative as IR

convertToPython :: [Statement] -> DocBuilder ()
convertToPython = foldM (\_ b -> writeBinding b) ()

writeBinding :: Statement -> DocBuilder ()
writeBinding (Decl name t) = do
  write $ prettyVarName name
  write " = "
  writeDefaultVarType t
  write "\n"
writeBinding (Assign name t val) = do
  write $ prettyVarName name
  write " = "
  write $ prettyExpr val
  write "\n"
writeBinding (For name start end body) = do
  write "for "
  write $ prettyVarName name
  write " in range("
  write $ prettyExpr start
  write ", "
  write $ prettyExpr end
  write "):\n"
  indented $ mapM_ writeBinding body
  write "\n"
writeBinding (MultiReadAssign sep parts) = do
  write "line = input()\n"
  write "parts = line.split("
  write (pretty $ toPyStrLit sep)
  write ")\n"
  mapM_ writeMultiReadAssignPart (NE.zip (fromList [0 ..]) parts)
  where
    writeMultiReadAssignPart :: (Int, (VarName, ReadType)) -> DocBuilder ()
    writeMultiReadAssignPart (idx, (name, rt)) = do
      write $ prettyVarName name
      write " = "
      write $ prettyReadAtom ("parts[" <> pretty idx <> "]") rt
      write "\n"
writeBinding (AppendToArray name _ val) = do
  write $ prettyVarName name
  write ".append("
  write (prettyExpr val)
  write ")\n"

prettyExpr :: Expr -> Doc'
prettyExpr (ConstInt i) = pretty i
prettyExpr (Var name) = prettyVarName name
prettyExpr (ReadAtom rt) = prettyReadAtom "input()" rt
prettyExpr (TupleOrStruct _ es) = tupled (prettyExpr <$> toList es)

toPyStrLit :: Text -> Text
toPyStrLit s = "'" <> s <> "'"

prettyReadAtom :: Doc' -> ReadType -> Doc'
prettyReadAtom source ReadInt = "int(" <> source <> ")"
prettyReadAtom source ReadFloat = "float(" <> source <> ")"
prettyReadAtom source ReadString = source

writeDefaultVarType :: VarType -> DocBuilder ()
writeDefaultVarType IntType = write "0"
writeDefaultVarType FloatType = write "0.0"
writeDefaultVarType StringType = write "''"
writeDefaultVarType (ArrayType _ t) = do
  write "[]"

prettyVarName :: VarName -> Doc'
prettyVarName (VarName ns) = prettyVarName' (toList ns)
  where
    prettyVarName' :: [Text] -> Doc'
    prettyVarName' [] = mempty
    prettyVarName' [n] = pretty n
    prettyVarName' (n : ns) = pretty n <> "_" <> prettyVarName' ns
