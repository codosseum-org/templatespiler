{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Templatespiler.ToLang.Monad where

import Control.Monad.Writer

newtype ToLangT w m a = ToLangT (WriterT w m a)
  deriving newtype (Functor, Applicative, Monad, MonadWriter w, MonadTrans)

type ToLang w a = ToLangT w Identity a

runToLangT :: ToLangT w m a -> m (a, w)
runToLangT (ToLangT wm) = runWriterT wm

runToLang :: ToLang w a -> (a, w)
runToLang = runIdentity . runToLangT

warn :: (Monoid (w item), Applicative w, Monad m) => item -> ToLangT (w item) m ()
warn = tell . pure
