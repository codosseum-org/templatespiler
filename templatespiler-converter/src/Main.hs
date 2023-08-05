module Main where

import Main.Utf8 qualified as Utf8
import Shower (shower)
import Language.Templatespiler.Par

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
    putStrLn ""
