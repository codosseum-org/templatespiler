{-# LANGUAGE DataKinds #-}

module Main where

import Language.Templatespiler.Parser (parseBindingList)
import Main.Utf8 qualified as Utf8
import Prettyprinter
import Prettyprinter.Render.Terminal (putDoc)
import Shower
import Templatespiler.Convert.Target (TargetLanguage (..), toIR)
import Templatespiler.IR.Imperative (prettyProgram)
import Text.Trifecta (parseFromFile)

main :: IO ()
main = do
  tokens <- parseFromFile parseBindingList "../test.tmpspl"
  case tokens of
    Nothing -> exitFailure
    Just bs -> do
      let ir = toIR @C bs
      putDoc $ prettyProgram ir
