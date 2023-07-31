{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Templatespiler.CodeGen where

import Templatespiler.AST (Block)

class CodeGen (lang :: Language) where
  codeGen :: Block -> Text

data Language
  = Java
  | C
  | Haskell