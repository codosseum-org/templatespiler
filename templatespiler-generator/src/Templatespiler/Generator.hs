module Templatespiler.Generator where

import Hedgehog.Gen (sample)
import Language.Templatespiler.Syntax
import Templatespiler.Generate

generateInput :: (MonadIO m) => BindingList -> m [Text]
generateInput = sample . arbitraryInput
