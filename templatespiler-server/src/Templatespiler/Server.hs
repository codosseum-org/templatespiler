{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Templatespiler.Server where

import Data.Aeson
import Data.Text (toLower)
import Data.UUID
import Data.UUID qualified as UUID
import Servant.API
import Templatespiler.Convert.Target (TargetLanguage (..))

type TemplatespilerAPI =
  "template"
    :> ( "parse" :> ReqBody '[JSON] TemplateParseRequest :> Post '[JSON] ParsedTemplate
          :<|> "generate" :> Capture "template_id" TemplateID :> QueryParam "amount" Int :> Get '[JSON] GenerateResponse
          :<|> "compile" :> Capture "template_id" TemplateID :> QueryParam "language" Language :> Get '[JSON] CompiledTemplateResponse
       )

data TemplateParseRequest = TemplateParseRequest
  { version :: Text
  , template :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON TemplateParseRequest
instance FromJSON TemplateParseRequest

newtype ParsedTemplate = ParsedTemplate TemplateID deriving newtype (Eq, Show, ToJSON)
newtype TemplateID = TemplateID UUID deriving newtype (Eq, Show)

instance FromHttpApiData TemplateID where
  parseUrlPiece = fmap TemplateID . parseUrlPiece

data GenerateResponse = GenerateResponse {inputs :: [[Text]]}
  deriving stock (Eq, Show, Generic)

data CompiledTemplateResponse = CompiledTemplateResponse
  { warnings :: [Text]
  , template :: CompiledTemplate
  }
  deriving stock (Eq, Show, Generic)

data CompiledTemplate = CompiledTemplate
  { language :: Language
  , code :: Text
  }
  deriving stock (Eq, Show, Generic)

newtype Language = Language TargetLanguage deriving newtype (Eq, Show, Generic)

instance FromHttpApiData Language where
  parseUrlPiece = fmap Language . parseTargetLanguage

parseTargetLanguage :: Text -> Either Text TargetLanguage
parseTargetLanguage (toLower -> t) = case t of
  "python" -> Right Python
  other -> Left $ "Unknown language: " <> other

instance ToJSON TemplateID where
  toJSON (TemplateID uuid) = toJSON $ UUID.toText uuid

instance ToJSON GenerateResponse
instance ToJSON CompiledTemplateResponse
instance ToJSON CompiledTemplate
instance ToJSON Language
