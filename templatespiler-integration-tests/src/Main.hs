{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently, mapConcurrently_)
import Control.Monad (forM_)
import Control.Monad.Trans.Resource
import Data.Map qualified as Map
import Data.Text.IO qualified as Text
import Hedgehog (Property, forAll, property)
import Language.Templatespiler.Parser
import Language.Templatespiler.Syntax
import Prettyprinter
import Prettyprinter.Render.Terminal
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO (hClose)
import System.Process
import Templatespiler.Convert
import Templatespiler.Convert.Target
import Templatespiler.Emit.Common
import Templatespiler.Generate (arbitraryInput)
import Test.Syd
import Test.Syd.Hedgehog ()
import Text.Trifecta
import "temporary-resourcet" System.IO.Temp qualified as TempResourceT

main :: IO ()
main = do
  getEnv "PATH" >>= putStrLn
  sydTest spec

-- | A helper function to run a ResourceT action and then call a test function with the result.
resourceTToAroundFunc :: ResourceT IO a -> (a -> IO ()) -> IO ()
resourceTToAroundFunc resourceT test = runResourceT $ do
  -- Run the resourceT action
  result <- resourceT
  -- Call the test function with the result
  liftIO $ test result

spec :: Spec
spec = describe "Integration Test" $ do
  templatespilerIntegrationTest "Simple Template with 3 Integers" simpleTemplate3Ints
  templatespilerIntegrationTest "Template from README" templateFromReadme
  templatespilerIntegrationTest "Template from README 2" templateFromReadme2
  templatespilerIntegrationTest "Template from README 3" templateFromReadme3

templatespilerIntegrationTestBody :: (BindingList, Map TargetLanguage ([Text] -> IO ())) -> Property
templatespilerIntegrationTestBody (res, runners) = property $ do
  input' <- forAll $ arbitraryInput res

  liftIO $ mapConcurrently_ (\lang -> (runners Map.! lang) input') [minBound @TargetLanguage ..]

templatespilerIntegrationTest :: String -> Text -> TestDefM outers () ()
templatespilerIntegrationTest name input =
  describe name $
    aroundAll (resourceTToAroundFunc $ compileTemplate input) $ do
      itWithOuter "runs correctly" templatespilerIntegrationTestBody

runProcessWithStdin :: Text -> [Text] -> [Text] -> IO ()
runProcessWithStdin processName args input = do
  output <- readProcess (toString processName) (toString <$> args) (toString $ unlines input)
  output `shouldBe` ""

parseTemplate :: Text -> Either Text BindingList
parseTemplate input = do
  let x = parseByteString parseBindingList mempty . encodeUtf8 $ input
  case x of
    Success a -> Right a
    Failure e -> Left $ renderStrict $ layoutPretty defaultLayoutOptions $ _errDoc e

compileTemplate :: Text -> ResourceT IO (BindingList, Map TargetLanguage ([Text] -> IO ()))
compileTemplate template = do
  -- Parse the template
  let parsed = parseTemplate template
  res <- case parsed of
    Left e -> liftIO $ fail $ toString e
    Right r -> pure r

  -- Generate and compile code for each language
  runners <-
    fromList
      <$> forM
        [minBound ..] -- every language
        ( \lang -> do
            -- Convert the parsed template to target language code
            code <- case convertTo res lang of
              Nothing -> fail $ "Conversion failed for " ++ show lang
              Just (ConversionFailed errorDoc) -> do
                fail $ toString $ renderStrict $ layoutPretty defaultLayoutOptions errorDoc
              Just (ConvertResult warnings code) -> do
                liftIO $ putDoc $ vsep warnings
                pure $ show code

            -- Compile the code and get a runner
            runner <- withCompiled lang code
            pure (lang, runner)
        )

  pure (res, runners)

withCompiled :: (MonadResource m) => TargetLanguage -> Text -> m ([Text] -> IO ())
withCompiled lang code = do
  -- Create a temporary directory
  (_, dir) <- TempResourceT.createTempDirectory Nothing "templatespiler"

  -- Determine the source file extension based on the language
  let sourceExt = case lang of
        Python -> ".py"
        C -> ".c"
        Haskell -> ".hs"
  -- Create and write to a temporary source file
  (_, sourceFp, sourceHandle) <- TempResourceT.openTempFile (Just dir) ("source" <> sourceExt)
  liftIO $ Text.hPutStrLn sourceHandle code
  liftIO $ hClose sourceHandle

  -- Compile the source file if necessary
  compiledFile <- case lang of
    Python -> pure sourceFp
    C -> do
      let outFile = dir </> "c.out"
      liftIO $ callProcess "gcc" [sourceFp, "-o", outFile]
      pure (fromString outFile)
    Haskell -> do
      let outFile = dir </> "hs.out"
      liftIO $ callProcess "ghc" [sourceFp, "-o", outFile]
      pure (fromString outFile)

  -- Define how to run the compiled program
  let (cmd, args) = case lang of
        Python -> ("python3", [toText compiledFile])
        C -> (toText compiledFile, [])
        Haskell -> (toText compiledFile, [])

  pure $ \inputs -> runProcessWithStdin cmd args inputs

simpleTemplate3Ints :: Text
simpleTemplate3Ints = "a : Integer\n b : Integer\n c : Integer\n"

templateFromReadme :: Text
templateFromReadme = "inputs: list (num : Integer)"

templateFromReadme2 :: Text
templateFromReadme2 = "start: sep-by \" \" [x : Integer y : Integer]\ninputs: list (sep-by \" \" [x : Integer y : Integer])"

templateFromReadme3 :: Text
templateFromReadme3 =
  unlines
    [ "prices: list (sep-by \" \" [item : String price : Float])"
    , "orders: list ["
    , "  name: String"
    , "  order: list (sep-by \" \" [quantity : Integer item : String])"
    , "  ]"
    ]
