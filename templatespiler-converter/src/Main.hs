module Main where

import Control.Monad.Writer (execWriter, runWriter)
import Language.Templatespiler.Par
import Main.Utf8 qualified as Utf8
import Prettyprinter.Render.Text
import Shower (shower)
import Templatespiler.Convert.Python (convertToPython)

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    file <- readFileText "../test.tmpspl"
    let tokens = myLexer file
    case pBindingList tokens of
      Left err -> putStrLn $ shower err
      Right bs -> do
        putStrLn "Parsed:"
        putStrLn $ shower bs
        putDoc $ execWriter $ convertToPython bs
    putStrLn ""
