module Main where

import Hedgehog.Gen (sample)
import Language.Templatespiler.Parser
import Main.Utf8 qualified as Utf8
import Templatespiler.Generate (arbitraryInput)
import Text.Trifecta (parseFromFile)

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    tokens <- parseFromFile parseBindingList "./test.tmpspl"
    case tokens of
      Nothing -> exitFailure
      Just bs -> do
        putStrLn ""
        i <- sample $ arbitraryInput bs
        traverse_ putTextLn i

    putStrLn ""
