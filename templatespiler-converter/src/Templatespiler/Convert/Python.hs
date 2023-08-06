module Templatespiler.Convert.Python where

import Control.Monad (foldM)
import Language.Templatespiler.Abs as Abs
import Prettyprinter (Doc, Pretty (pretty), hang)
import Templatespiler.Convert.Common

convertToPython :: BindingList -> DocBuilder ()
convertToPython (BindingList _ bindings) = foldM (\_ b -> writeBinding b) () bindings

writeBinding :: Binding -> DocBuilder ()
writeBinding (Binding _ n t) = do
  tell (identToDoc n)
  tell " = "
  writeType t
  tell "\n"

writeType :: Abs.Type -> DocBuilder ()
writeType (Abs.StringType _) = tell "input()"
writeType (Abs.IntegerType _) = tell "int(input())"
writeType (Abs.FloatType _) = tell "float(input())"
writeType (Abs.CombinatorType _ combin) = writeCombinator combin

writeCombinator :: Combinator -> DocBuilder ()
writeCombinator (ParenCombinator _ c) = writeCombinator c
writeCombinator (ArrayCombinator _ len c) = do
  tell "for i"
  tell " in range(0, "

  writeVarOrConstInt len
  tell "):\n"
  indented $ do
    writeBindingOrCombinator c
writeCombinator (ListCombinator _ c) = do
  tell "length = int(input())\n"
  tell "for i in range(0, length):\n"
  indented $ do
    writeBindingOrCombinator c
writeCombinator other = error ("writeCombinator: " <> show other)

writeBindingOrCombinator :: BindingOrCombinator -> DocBuilder ()
writeBindingOrCombinator (NamedBinding _ b) = writeBinding b
writeBindingOrCombinator (ParenBinding _ b) = writeBinding b
writeBindingOrCombinator (UnnamedBinding _ c) = writeCombinator c

writeVarOrConstInt :: VarOrConstInt -> DocBuilder ()
writeVarOrConstInt (ConstInt _ v) = tell (pretty v)
writeVarOrConstInt (ConstVar _ v) = tell (identToDoc v)
