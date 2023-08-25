module Main where

import Language.Templatespiler.Par
import Main.Utf8 qualified as Utf8
import Prettyprinter (Doc)
import Prettyprinter.Render.Text
import Shower (printer, shower)
import Templatespiler.Convert (toImperative)
import Templatespiler.Convert.C (convertToC)
import Templatespiler.Convert.Common (runConversion)
import Templatespiler.Convert.Python (convertToPython)

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    file <- readFileText "../test.tmpspl"
    let tokens = myLexer file
    case pBindingList tokens of
      Left err -> putStrLn $ shower err
      Right bs -> do
        -- putStrLn "Parsed:"
        -- putStrLn $ shower bs
        putStrLn ""
        let imp = toImperative bs
        printer imp
        putStrLn "Python: "
        putConversion $ runConversion $ convertToPython imp
        putStrLn "C: "
        putConversion $ runConversion $ convertToC imp
    putStrLn ""

putConversion :: (Foldable t, Show a) => (Doc ann, t a) -> IO ()
putConversion (doc, warnings) = do
  putDoc doc
  putStrLn ""
  putStrLn "Warnings:"
  mapM_ (putStrLn . shower) warnings
  putStrLn ""
