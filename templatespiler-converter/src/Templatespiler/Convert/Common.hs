{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}

module Templatespiler.Convert.Common (
  tell,
  identToText,
  identToDoc,
  indented,
  curlyBlock,
  indentDepth,
  Doc',
  DocBuilder,
  FallibleDocBuilder,
  MonadDocBuilder (..),
  runConversion,
  fallibleToDocBuilder,
  fallibleToDoc,
  readTypeToVarType,
  toFallible,
) where

import Control.Monad.Writer
import Language.Templatespiler.Abs (Ident (..))
import Prettyprinter (Doc, Pretty (pretty), indent, vcat, vsep)
import Templatespiler.Convert.Targets (TargetLanguageWarning)
import Templatespiler.Convert.Warning
import Templatespiler.IR.Imperative (Expr, ReadType (..), VarType (..))
import Prelude hiding (group)

type Ann = ()

type Doc' = Doc Ann

newtype DocBuilder target a = DocBuilder {runDocBuilder :: WriterT (Doc Ann) (Writer [TargetLanguageWarning target]) a}
  deriving newtype (Functor, Applicative, Monad, MonadWriter (Doc Ann))

newtype FallibleDocBuilder target a = FallibleDocBuilder
  { runFallibleDocBuilder ::
      WriterT (Doc Ann) (MaybeT (Writer [TargetLanguageWarning target])) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadWriter (Doc Ann), MonadFail)

runConversion :: DocBuilder target a -> (Doc Ann, [TargetLanguageWarning target])
runConversion = runWriter . execWriterT . runDocBuilder

identToText :: Ident -> Text
identToText (Ident t) = t

identToDoc :: Ident -> Doc Ann
identToDoc = pretty . identToText

indentDepth :: Int
indentDepth = 4

-- | Indents all the content of the given 'DocBuilder' by 'indentDepth'.
indented :: DocBuilder target a -> DocBuilder target a
indented b = do
  let ((a, doc), warnings) = runWriter . runWriterT . runDocBuilder $ b
  traverse_ warn warnings

  tell (indent indentDepth doc)

  pure a

curlyBlock :: (MonadDocBuilder m) => [Doc Ann] -> m ()
curlyBlock b =
  write $
    vsep
      [ "{"
      , indent indentDepth (vcat b)
      , "}"
      ]

class (Monad m) => MonadDocBuilder m where
  write :: Doc' -> m ()
  warn :: TargetLanguageWarning target -> m ()

  fromFallible :: FallibleDocBuilder target a -> m ()

  listenOnly :: m a -> m (a, Doc')
  listenOnly' :: m a -> m Doc'
  listenOnly' = fmap snd . listenOnly

instance MonadDocBuilder (DocBuilder target) where
  write = DocBuilder . tell
  warn = DocBuilder . lift . tell . pure

  fromFallible = void . fallibleToDocBuilder

  listenOnly b = do
    let ((a, doc), warnings) = runWriter . runWriterT . runDocBuilder $ b
    traverse_ warn warnings
    pure (a, doc)

instance MonadDocBuilder (FallibleDocBuilder target) where
  write = FallibleDocBuilder . tell
  warn = FallibleDocBuilder . lift . tell . pure
  fromFallible = void

  listenOnly b = do
    let (x, warnings) = runWriter . runMaybeT . runWriterT . runFallibleDocBuilder $ b
    traverse_ warn warnings
    case x of
      Nothing -> fail ""
      Just (a, doc) -> pure (a, doc)

toFallible :: DocBuilder target a -> FallibleDocBuilder target a
toFallible b = do
  let ((a, doc), warnings) = runWriter . runWriterT . runDocBuilder $ b
  traverse_ warn warnings
  tell doc
  pure a

fallibleToDocBuilder :: FallibleDocBuilder target a -> DocBuilder target (Maybe a)
fallibleToDocBuilder b = do
  let (x, w) = runWriter . runMaybeT . runWriterT . runFallibleDocBuilder $ b
  traverse_ warn w
  whenJust x (tell . snd)

  pure (fst <$> x)

fallibleToDoc :: FallibleDocBuilder target a -> DocBuilder target (Maybe Doc')
fallibleToDoc b = do
  let (x, w) = runWriter . runMaybeT . runWriterT . runFallibleDocBuilder $ b
  traverse_ warn w
  pure (snd <$> x)

readTypeToVarType :: ReadType -> VarType
readTypeToVarType ReadInt = IntType
readTypeToVarType ReadFloat = FloatType
readTypeToVarType ReadString = StringType
