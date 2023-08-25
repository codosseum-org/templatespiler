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
  TargetLanguage (..),
  ConvertWarning (..),
  MonadDocBuilder (..),
  write,
  warn,
  runConversion,
  fallibleToDocBuilder,
  fallibleToDoc,
  readTypeToVarType,
  toFallible,
) where

import Control.Monad.Writer
import Language.Templatespiler.Abs (Ident (..))
import Prettyprinter (Doc, Pretty (pretty), align, fillSep, group, hang, hardline, hcat, hsep, indent, line, nest, vcat, vsep)
import Shower (shower)
import Templatespiler.IR.Imperative (Expr, ReadType (..), VarType (..))
import Prelude hiding (group)

type Ann = ()

type Doc' = Doc Ann

newtype DocBuilder a = DocBuilder {runDocBuilder :: WriterT (Doc Ann) (Writer [ConvertWarning]) a}
  deriving newtype (Functor, Applicative, Monad, MonadWriter (Doc Ann))

newtype FallibleDocBuilder a = FallibleDocBuilder
  { runFallibleDocBuilder ::
      WriterT (Doc Ann) (MaybeT (Writer [ConvertWarning])) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadWriter (Doc Ann), MonadFail)

runConversion :: DocBuilder a -> (Doc Ann, [ConvertWarning])
runConversion = runWriter . execWriterT . runDocBuilder

data TargetLanguage = Python | C deriving (Show)
data ConvertWarning
  = CantConvertType VarType TargetLanguage
  | CantConvertExpr Expr TargetLanguage
  deriving (Show)

identToText :: Ident -> Text
identToText (Ident t) = t

identToDoc :: Ident -> Doc Ann
identToDoc = pretty . identToText

indentDepth :: Int
indentDepth = 4

-- | Indents all the content of the given 'DocBuilder' by 'indentDepth'.
indented :: DocBuilder a -> DocBuilder a
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
  warn :: ConvertWarning -> m ()

  fromFallible :: FallibleDocBuilder a -> m ()

  listenOnly :: m a -> m (a, Doc')
  listenOnly' :: m a -> m Doc'
  listenOnly' = fmap snd . listenOnly

instance MonadDocBuilder DocBuilder where
  write = DocBuilder . tell
  warn = DocBuilder . lift . tell . pure

  fromFallible = void . fallibleToDocBuilder

  listenOnly b = do
    let ((a, doc), warnings) = runWriter . runWriterT . runDocBuilder $ b
    traverse_ warn warnings
    pure (a, doc)

instance MonadDocBuilder FallibleDocBuilder where
  write = FallibleDocBuilder . tell
  warn = FallibleDocBuilder . lift . tell . pure
  fromFallible = void

  listenOnly b = do
    let (x, warnings) = runWriter . runMaybeT . runWriterT . runFallibleDocBuilder $ b
    traverse_ warn warnings
    case x of
      Nothing -> fail ""
      Just (a, doc) -> pure (a, doc)

toFallible :: DocBuilder a -> FallibleDocBuilder a
toFallible b = do
  let ((a, doc), warnings) = runWriter . runWriterT . runDocBuilder $ b
  traverse_ warn warnings
  tell doc
  pure a

fallibleToDocBuilder :: FallibleDocBuilder a -> DocBuilder (Maybe a)
fallibleToDocBuilder b = do
  let (x, w) = runWriter . runMaybeT . runWriterT . runFallibleDocBuilder $ b
  traverse_ warn w
  whenJust x (tell . snd)

  pure (fst <$> x)

fallibleToDoc :: FallibleDocBuilder a -> DocBuilder (Maybe Doc')
fallibleToDoc b = do
  let (x, w) = runWriter . runMaybeT . runWriterT . runFallibleDocBuilder $ b
  traverse_ warn w
  pure (snd <$> x)

readTypeToVarType :: ReadType -> VarType
readTypeToVarType ReadInt = IntType
readTypeToVarType ReadFloat = FloatType
readTypeToVarType ReadString = StringType
