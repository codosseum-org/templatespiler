module Main where

import Language.Templatespiler.Parser (parseBindingList)
import Text.Trifecta (parseFromFile)

main :: IO ()
main = do
  tokens <- parseFromFile parseBindingList "./test.tmpspl"
  print tokens
