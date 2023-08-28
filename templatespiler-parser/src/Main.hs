module Main where

import Data.Maybe (fromJust)
import Language.Templatespiler.Parser (parseBindingList, parseIdent)
import Language.Templatespiler.Pretty (prettyBindingList)
import Prettyprinter.Render.Terminal (putDoc)
import Shower (printer)
import Text.Trifecta (Parsing (eof), parseFromFile)

main :: IO ()
main = do
  tokens <- parseFromFile parseBindingList "../test.tmpspl"
  -- printer tokens
  putDoc $ prettyBindingList (fromJust tokens)
  putStrLn ""
