module Main where

import Main.Utf8 qualified as Utf8
import Shower (shower)
import Templatespiler.Parser (parseBlock)
import Text.Megaparsec (eof, errorBundlePretty, runParser)

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    contents <- readFileText "test.tmpspl"
    case (runParser (parseBlock <* eof) "test.tmpspl" contents) of
      Left err -> putStrLn $ errorBundlePretty err
      Right ast -> putStrLn $ shower ast
