{-# LANGUAGE DataKinds #-}

module Main where

import Language.Templatespiler.Parser (parseBindingList)
import Main.Utf8 qualified as Utf8
import Prettyprinter
import Prettyprinter.Render.Terminal (putDoc)
import Prettyprinter.Render.Text qualified as PP
import Shower
import Templatespiler.Convert.Target (TargetLanguage (..), toIR)
import Templatespiler.Emit.Target
import Templatespiler.IR.Imperative (prettyProgram)
import Templatespiler.ToLang.Target (ToLang (toLang))
import Text.Trifecta (parseFromFile)

main :: IO ()
main = do
  tokens <- parseFromFile parseBindingList "../test.tmpspl"
  case tokens of
    Nothing -> exitFailure
    Just bs -> do
      let ir = toIR @Python bs
      putDoc $ prettyProgram ir
      let py = toLang @Python ir
      putStrLn ""
      printer py
      let doc = emitLang @Python py
      PP.putDoc doc
      putStrLn ""
