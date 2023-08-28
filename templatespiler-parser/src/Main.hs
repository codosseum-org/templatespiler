module Main where

import Language.Templatespiler.Parser (parseBindingList, parseIdent)
import Shower (printer)
import Text.Trifecta (Parsing (eof), parseFromFile)

main :: IO ()
main = do
  tokens <- parseFromFile parseBindingList "../test.tmpspl"
  printer tokens
