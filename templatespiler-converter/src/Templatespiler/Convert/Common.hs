module Templatespiler.Convert.Common (tell, identToText, identToDoc, indented, indentDepth, DocBuilder) where

import Control.Monad.Writer
import Language.Templatespiler.Abs (Ident (..))
import Prettyprinter (Doc, Pretty (pretty), indent, nest)

type Ann = ()

type DocBuilder a = Writer (Doc Ann) a

identToText :: Ident -> Text
identToText (Ident t) = t

identToDoc :: Ident -> Doc Ann
identToDoc = pretty . identToText

indentDepth :: Int
indentDepth = 2

-- | Indents all the content of the given 'DocBuilder' by 'indentDepth'.
indented :: DocBuilder a -> DocBuilder a
indented b = do
  let (d, a) = runWriter b
  tell (indent indentDepth a)
  pure d