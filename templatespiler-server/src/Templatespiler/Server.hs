{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoPatternSynonyms #-}

module Templatespiler.Server where

import Control.Lens
import Data.Aeson
import Data.Base64.Types
import Data.OpenApi (HasVersion (version), License (..), NamedSchema (..), OpenApi, OpenApiType (..), Referenced (..), ToParamSchema (..), declareResponse)
import Data.OpenApi.Lens
import Data.OpenApi.Schema
import Data.Text (toLower)
import Data.Text.Encoding.Base64
import Data.UUID
import Data.UUID qualified as UUID
import Servant.API
import Servant.OpenApi
import Templatespiler.Convert.Target (TargetLanguage (..))

type TemplatespilerAPI =
  "template"
    :> ( "parse" :> ReqBody '[JSON] TemplateParseRequest :> Post '[JSON] ParsedTemplate
          :<|> "generate"
            :> Capture "template_id" TemplateID
            :> QueryParam' '[Required, Strict] "amount" Int
            :> Get '[JSON] GenerateResponse
          :<|> "compile"
            :> Capture "template_id" TemplateID
            :> QueryParam' '[Required, Strict] "language" Language
            :> Get '[JSON] CompiledTemplateResponse
       )

type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

type Api = TemplatespilerAPI :<|> SwaggerAPI

tsOpenAPI :: OpenApi
tsOpenAPI =
  toOpenApi (Proxy :: Proxy TemplatespilerAPI)
    & info . title .~ "Templatespiler API"
    & info . Data.OpenApi.version .~ "0.1.0"
    & info . description ?~ "REST API for generating code & inputs from the Templatespiler language"
    & info . license ?~ License "Affero General Public License 3.0 or later" Nothing

data TemplateParseRequest = TemplateParseRequest
  { version :: Text
  , template :: Base64String
  }
  deriving stock (Eq, Show, Generic)
instance ToSchema TemplateParseRequest where
  declareNamedSchema proxy = do
    -- okResponse <- declareResponse "application/json" (Proxy @ParsedTemplate)

    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . title ?~ "Submit a template for parsing"
      & mapped . schema . description ?~ "Submit a template for parsing, returning a unique ID that can be used to process the parsed template in other ways"
      & mapped . schema . properties . at "version" . _Just . _Inline . description ?~ "Version of the Templatespiler Language that the template is written in"
      & mapped . schema . properties . at "template" . _Just . _Inline . description ?~ "Content of the template, described using the Templatespiler Language, encoded as Base64"
      & mapped . schema . required .~ ["version", "template"]

-- & mapped . schema . at 200 ?~ Inline okResponse

newtype Base64String = Base64String Text deriving newtype (Eq, Show, ToJSON)
instance ToSchema Base64String where
  declareNamedSchema _ =
    declareNamedSchema (Proxy @Text)
      & mapped . schema . title ?~ "Base64 encoded string"

unBase64 :: Base64String -> Either Text Text
unBase64 (Base64String t) = decodeBase64Untyped t

toBase64 :: Text -> Base64String
toBase64 = Base64String . extractBase64 . encodeBase64

instance FromJSON TemplateParseRequest where
  parseJSON = withObject "TemplateParseRequest" $ \o ->
    TemplateParseRequest <$> o .: "version" <*> (Base64String <$> o .: "template")

newtype ParsedTemplate = ParsedTemplate TemplateID deriving newtype (Eq, Show, ToJSON)
instance ToSchema ParsedTemplate where
  declareNamedSchema _ = do
    pure $
      NamedSchema (Just "ParsedTemplate") $
        toParamSchema (Proxy @TemplateID)

newtype TemplateID = TemplateID UUID deriving newtype (Eq, Show, Ord)
instance ToParamSchema TemplateID where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & format ?~ "uuid"
      & pattern ?~ "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
      & minLength ?~ 36
      & maxLength ?~ 36

instance FromHttpApiData TemplateID where
  parseUrlPiece = fmap TemplateID . parseUrlPiece

newtype GenerateResponse = GenerateResponse {inputs :: [[Text]]}
  deriving stock (Eq, Show, Generic)
instance ToSchema GenerateResponse

data CompiledTemplateResponse = CompiledTemplateResponse
  { warnings :: [Text]
  , template :: CompiledTemplate
  }
  deriving stock (Eq, Show, Generic)

instance ToSchema CompiledTemplateResponse

data CompiledTemplate = CompiledTemplate
  { language :: Language
  , code :: Base64String
  }
  deriving stock (Eq, Show, Generic)

instance ToSchema CompiledTemplate

newtype Language = Language TargetLanguage deriving newtype (Eq, Show, Generic)
instance ToSchema Language
instance ToParamSchema Language

instance FromHttpApiData Language where
  parseUrlPiece = fmap Language . parseTargetLanguage

parseTargetLanguage :: Text -> Either Text TargetLanguage
parseTargetLanguage (toLower -> t) = case t of
  "python" -> Right Python
  other -> Left $ "Unknown language: " <> other <> ". Supported languages: python"

instance ToJSON TemplateID where
  toJSON (TemplateID uuid) = toJSON $ UUID.toText uuid

instance ToJSON GenerateResponse
instance ToJSON CompiledTemplateResponse
instance ToJSON CompiledTemplate
instance ToJSON Language
