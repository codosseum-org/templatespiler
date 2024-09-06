{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Map.Strict (insert, lookup)

import Data.OpenApi
import Data.UUID.V4 (nextRandom)
import Language.Templatespiler.Parser (parseBindingList)
import Language.Templatespiler.Syntax (BindingList)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Prettyprinter
import Prettyprinter.Render.Text
import Servant (Application, Handler, ServerError (..), ServerT, err400, hoistServer, serve, throwError, (:<|>) (..))
import Servant.OpenApi
import Templatespiler.Convert (convertTo, renderConvertResult)
import Templatespiler.Emit.Common (ConvertResult (..), PDoc, TDoc)
import Templatespiler.Generator (generateInput)
import Templatespiler.Server
import Text.Trifecta (ErrInfo (_errDoc), Result (..), parseByteString)
import Prelude hiding (State, state)

main :: IO ()
main = do
  initialState <- State <$> newTVarIO mempty
  putStrLn "Starting server on port 8080..."

  run 8080 $ app initialState
  putStrLn ""

tsOpenAPI :: OpenApi
tsOpenAPI = toOpenApi (Proxy :: Proxy TemplatespilerAPI)

app :: State -> Application
app s = simpleCors $ serve (Proxy @TemplatespilerAPI) $ hoistServer (Proxy @TemplatespilerAPI) (nt s) templatespilerServer

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

newtype State = State
  { templates :: TVar (Map TemplateID BindingList)
  }
type AppM = ReaderT State Handler

templatespilerServer :: ServerT TemplatespilerAPI AppM
templatespilerServer =
  parse :<|> generate :<|> compile
  where
    parse :: TemplateParseRequest -> AppM ParsedTemplate
    parse TemplateParseRequest {..} = do
      template' <- either (\err -> throwError $ err400 {errBody = "Bad Base64 Template: " <> encodeUtf8 err}) pure (unBase64 template)

      state <- ask
      let parsed = parseByteString parseBindingList mempty (encodeUtf8 template')
      case parsed of
        Success bindingList -> do
          templateID <- TemplateID <$> liftIO nextRandom
          atomically $ modifyTVar' (templates state) (insert templateID bindingList)
          pure $ ParsedTemplate templateID
        Failure err -> throwError $ err400 {errBody = encodeUtf8 $ renderStrict $ layoutPretty defaultLayoutOptions $ _errDoc err}
    generate :: TemplateID -> Int -> AppM GenerateResponse
    generate templateId len = do
      state <- ask
      templates' <- readTVarIO (templates state)
      case lookup templateId templates' of
        Nothing -> throwError $ err400 {errBody = "Template not found"}
        Just bindingList -> do
          inputs <- liftIO $ replicateM len $ generateInput bindingList
          pure $ GenerateResponse inputs
    compile :: TemplateID -> Language -> AppM CompiledTemplateResponse
    compile templateId l@(Language lang) = do
      state <- ask
      templates' <- readTVarIO (templates state)
      case lookup templateId templates' of
        Nothing -> throwError $ err400 {errBody = "Template not found"}
        Just bindingList -> do
          liftIO $ putStrLn $ "Compiling template " <> show templateId <> " to " <> show l
          let compiled = convertTo bindingList lang
          case compiled of
            Nothing -> throwError $ err400 {errBody = "Language not supported"}
            Just compileResult -> do
              liftIO $ putStrLn $ "Compiled template " <> show templateId <> " to " <> show l
              liftIO $ putTextLn $ renderConvertResult compileResult
              case compileResult of
                ConversionFailed doc -> throwError $ err400 {errBody = encodeUtf8 $ renderPDoc doc}
                ConvertResult warnings code -> pure $ CompiledTemplateResponse (renderPDoc <$> warnings) (CompiledTemplate l (toBase64 (renderTDoc code)))

renderPDoc :: PDoc -> Text
renderPDoc = renderTDoc . unAnnotate

renderTDoc :: TDoc -> Text
renderTDoc = renderStrict . layoutPretty defaultLayoutOptions
