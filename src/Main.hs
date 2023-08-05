module Main where

import Main.Utf8 qualified as Utf8
import Shower (shower)
import Text.Megaparsec (eof, errorBundlePretty, runParser)
import Language.Templatespiler.Par

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    putStrLn "🚀"
