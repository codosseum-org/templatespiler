module Util where

import Test.Syd

shouldBeRight :: (Show a, HasCallStack) => Either Text a -> IO a
shouldBeRight theEither = do
  shouldSatisfyNamed theEither "IsRight" isRight
  Right a <- pure theEither
  pure a

shouldBeJust :: (Show a, HasCallStack) => Text -> Maybe a -> IO a
shouldBeJust annotation theMaybe = do
  shouldSatisfyNamed theMaybe (toString ("IsJust: " <> annotation)) isJust
  Just a <- pure theMaybe
  pure a