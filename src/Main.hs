module Main where

import Main.Utf8 qualified as Utf8

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    putTextLn "Hello 🌎"
