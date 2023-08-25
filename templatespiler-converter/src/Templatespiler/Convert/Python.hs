module Templatespiler.Convert.Python where

import Control.Monad (foldM)
import Prettyprinter (Pretty (pretty))
import Templatespiler.Convert.Common
import Templatespiler.IR.Imperative as IR

convertToPython :: [Statement] -> DocBuilder ()
convertToPython = foldM (\_ b -> writeBinding b) ()

writeBinding :: Statement -> DocBuilder ()
writeBinding (Decl name t) = do
  writeVarName name
  write " = "
  writeDefaultVarType t
  write "\n"
writeBinding (Assign name t val) = do
  writeVarName name
  write " = "
  writeExpr val
  write "\n"
writeBinding (For name start end body) = do
  write "for "
  writeVarName name
  write " in range("
  writeExpr start
  write ", "
  writeExpr end
  write "):\n"
  indented $ mapM_ writeBinding body
  write "\n"
writeBinding (MultiReadAssign sep parts) = do
  write "line = input()\n"
  write "parts = line.split("
  write (pretty $ toPyStrLit sep)
  write ")\n"
  mapM_ writeMultiReadAssignPart (zip [0 ..] parts)
  where
    writeMultiReadAssignPart :: (Int, (VarName, ReadType)) -> DocBuilder ()
    writeMultiReadAssignPart (idx, (name, rt)) = do
      writeVarName name
      write " = "
      writeReadAtom ("parts[" <> pretty idx <> "]") rt
      write "\n"

writeExpr :: Expr -> DocBuilder ()
writeExpr (ConstInt i) = write (pretty i)
writeExpr (Var name) = writeVarName name
writeExpr (ReadAtom rt) = writeReadAtom "input()" rt

toPyStrLit :: Text -> Text
toPyStrLit s = "'" <> s <> "'"

writeReadAtom :: Doc' -> ReadType -> DocBuilder ()
writeReadAtom source ReadInt = write ("int(" <> source <> ")")
writeReadAtom source ReadFloat = write ("float(" <> source <> ")")
writeReadAtom source ReadString = write source

writeDefaultVarType :: VarType -> DocBuilder ()
writeDefaultVarType IntType = write "0"
writeDefaultVarType FloatType = write "0.0"
writeDefaultVarType StringType = write "''"
writeDefaultVarType (ArrayType t) = do
  write "[]"

writeVarName :: VarName -> DocBuilder ()
writeVarName (VarName ns) = writeVarName' (toList ns)
  where
    writeVarName' :: [Text] -> DocBuilder ()
    writeVarName' [] = pass
    writeVarName' [n] = write (pretty n)
    writeVarName' (n : ns) = do
      write (pretty n)
      write "_"
      writeVarName' ns
