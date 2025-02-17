module Templatespiler.Emit.Common where

import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)

indentDepth :: Int
indentDepth = 4

type PDoc = Doc AnsiStyle

data ConvertResult
  = ConversionFailed PDoc
  | ConvertResult
      { warnings :: [PDoc]
      , code :: PDoc
      }
  deriving stock (Show)
