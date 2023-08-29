{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Templatespiler.Server where

import Data.Aeson
import Data.UUID
import Servant.API
import Templatespiler.Convert.Target (TargetLanguage)

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

newtype ParsedTemplate = ParsedTemplate TemplateID
newtype TemplateID = TemplateID UUID

data GenerateResponse where
  GenerateResponse :: {inputs :: [[Text]]} -> GenerateResponse

data CompiledTemplateResponse = CompiledTemplateResponse
  { warnings :: [Text]
  , template :: CompiledTemplate
  }

data CompiledTemplate = CompiledTemplate
  { language :: Language
  , code :: Text
  }
newtype Language = Language TargetLanguage
