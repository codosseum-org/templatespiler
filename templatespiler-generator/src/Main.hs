module Main where

import Hedgehog.Gen (sample)
import Language.Templatespiler.Par
import Main.Utf8 qualified as Utf8
import Prettyprinter (Doc)
import Prettyprinter.Render.Text
import Shower (printer, shower)
import Templatespiler.Generate (arbitraryInput)

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    file <- readFileText "../test.tmpspl"
    let tokens = myLexer file
    case pBindingList tokens of
      Left err -> putStrLn $ shower err
      Right bs -> do
        putStrLn ""
        i <- sample $ arbitraryInput bs
        traverse_ putTextLn i

    putStrLn ""
