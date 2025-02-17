{-# LANGUAGE DataKinds #-}

module Main where

import Language.Templatespiler.Parser (parseBindingList)
import Main.Utf8 qualified as Utf8
import Prettyprinter
import Prettyprinter.Render.Terminal (putDoc)
import Prettyprinter.Render.Text qualified as PP
import Templatespiler.Convert (renderConvertResult)
import Templatespiler.Convert.Target (TargetLanguage (..), toIR)
import Templatespiler.Emit.Target
import Templatespiler.IR.Imperative
import Templatespiler.ToLang.Target (ToLang (toLang))
import Text.Trifecta (parseFromFile)

main :: IO ()
main = do
  tokens <- parseFromFile parseBindingList "./test.tmpspl"
  case tokens of
    Nothing -> exitFailure
    Just bs -> do
      let ir = toIR @Python bs
      -- putDoc $ prettyProgram ir
      let py = toLang @Python ir
      let c = toLang @C ir
      putStrLn ""

      let doc = emitLang @Python py
      -- PP.putDoc doc
      putStrLn ""
      let doc2 = emitLang @C c
      putTextLn $ renderConvertResult doc2
      putStrLn ""

      let toHaskell = toLang @Haskell bs
      let doc3 = emitLang @Haskell toHaskell
      putTextLn $ renderConvertResult doc3
      putStrLn ""
