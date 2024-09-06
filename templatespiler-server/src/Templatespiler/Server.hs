{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoPatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Templatespiler.Server where

import Control.Lens
import Data.Aeson
import Data.Base64.Types
import Data.OpenApi (NamedSchema (..), OpenApiType (..), ToParamSchema (..))
import Data.OpenApi.Lens
import Data.OpenApi.Schema
import Data.Text (toLower)
import Data.Text.Encoding.Base64
import Data.UUID
import Data.UUID qualified as UUID
import Servant.API
import Templatespiler.Convert.Target (TargetLanguage (..))

type TemplatespilerAPI =
  "template"
    :> ( "parse" :> ReqBody '[JSON] TemplateParseRequest :> Post '[JSON] ParsedTemplate
          :<|> "generate" :> Capture "template_id" TemplateID :> QueryParam' '[Required, Strict] "amount" Int :> Get '[JSON] GenerateResponse
          :<|> "compile" :> Capture "template_id" TemplateID :> QueryParam' '[Required, Strict] "language" Language :> Get '[JSON] CompiledTemplateResponse
       )

data TemplateParseRequest = TemplateParseRequest
  { version :: Text
  , template :: Base64String
  }
  deriving stock (Eq, Show, Generic)
instance ToSchema TemplateParseRequest

newtype Base64String = Base64String Text deriving newtype (Eq, Show, ToJSON)
instance ToSchema Base64String where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

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

data GenerateResponse = GenerateResponse {inputs :: [[Text]]}
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
