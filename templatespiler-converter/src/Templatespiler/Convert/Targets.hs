{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Templatespiler.Convert.Targets where

data TargetLanguage = Python | C deriving stock (Show)

type family TargetLanguageWarning (a :: TargetLanguage)
