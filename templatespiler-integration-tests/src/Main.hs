{-# LANGUAGE PackageImports #-}

module Main where

import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Resource
import Data.Text.IO qualified as Text
import Hedgehog (Property, evalEither, evalMaybe, forAll, property)
import Hedgehog.Internal.Property (failWith)
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

spec :: Spec
spec = describe "Integration Test" $ do
  it "Simple Template" $ templatespilerIntegrationTest simpleTemplate3Ints
  it "Readme Template" $ templatespilerIntegrationTest templateFromReadme

templatespilerIntegrationTest :: Text -> Property
templatespilerIntegrationTest input = property $ hoist runResourceT $ do
  let parsed = parseTemplate input
  res <- evalEither parsed

  let allLanguages = [minBound ..] :: [TargetLanguage]
  for_ allLanguages $ \lang -> do
    genResult <- evalMaybe $ convertTo res lang
    code <- case genResult of
      ConversionFailed errorDoc -> failWith Nothing $ toString $ renderStrict $ layoutPretty defaultLayoutOptions errorDoc
      ConvertResult warnings code -> do
        liftIO $ putDoc $ vsep warnings
        pure $ show code

    exec <- withCompiled lang code
    input' <- forAll $ arbitraryInput res
    liftIO $ exec input'

withCompiled :: (MonadResource m) => TargetLanguage -> Text -> m ([Text] -> IO ())
withCompiled lang code = do
  (_, fp) <- TempResourceT.createTempDirectory Nothing "templatespiler"

  let sourceExt = case lang of
        Python -> ".py"
        C -> ".c"
  (_, sourceFp, sourceHandle) <- TempResourceT.openTempFile (Just fp) ("source" <> sourceExt)
  liftIO $ Text.hPutStrLn sourceHandle code
  liftIO $ hClose sourceHandle
  compiledFile <- case lang of
    Python -> pure sourceFp
    C -> do
      liftIO $ callProcess "gcc" [sourceFp, "-o", fp </> "a.out"]
      pure $ fp <> "/a.out"

  let (cmdToRun, argsToRun) = case lang of
        Python -> ("python3", [toText compiledFile])
        C -> (toText compiledFile, [])

  pure $ \inputs -> runProcessWithStdin cmdToRun argsToRun inputs

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

simpleTemplate3Ints :: Text
simpleTemplate3Ints = "a : Integer\n b : Integer\n c : Integer\n"

templateFromReadme :: Text
templateFromReadme = "inputs: list (num : Integer)"
