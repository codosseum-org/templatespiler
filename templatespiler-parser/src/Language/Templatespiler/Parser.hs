module Language.Templatespiler.Parser where

import Data.Text (pack)
import Language.Templatespiler.Syntax
import Text.Trifecta
import Prelude hiding (Type)

isHSpace :: Char -> Bool
isHSpace c = c == ' ' || c == '\t'

skipHSpaces :: (CharParsing m) => m ()
skipHSpaces = skipSome (satisfy isHSpace)

parseBindingList :: Parser BindingList
parseBindingList = BindingList . fromList <$> some parseBinding

parseBinding :: Parser Binding
parseBinding = do
  name <- token parseIdent
  _ <- symbolic ':'
  Binding name <$> parseType

parseIdent :: Parser Ident
parseIdent = Ident . toText <$> some letter

parseType :: Parser Type
parseType =
  try parseCombinatorType
    <|> (TerminalType <$> try parseTerminalType)

parseTerminalType :: Parser TerminalType
parseTerminalType =
  IntType <$ symbol "Integer"
    <|> FloatType <$ symbol "Float"
    <|> StringType <$ symbol "String"

parseCombinatorType :: Parser Type
parseCombinatorType = CombinatorType <$> parseCombinator

parseCombinator :: Parser Combinator
parseCombinator =
  parseGroupCombinator
    <|> try (parens parseCombinator)
    <|> try parseArrayCombinator
    <|> try parseSepByCombinator
    <|> try parseListCombinator

parseNamedCombinator :: Parser Combinator
parseNamedCombinator = do
  name <- parseIdent
  _ <- symbol ":"
  NamedCombinator name <$> parseType

parseListCombinator :: Parser Combinator
parseListCombinator = do
  _ <- symbol "list"
  ListCombinator <$> parseType

parseGroupCombinator :: Parser Combinator
parseGroupCombinator = do
  _ <- symbol "["
  l <- parseBindingList
  _ <- symbol "]"
  pure $ GroupCombinator l

parseArrayCombinator :: Parser Combinator
parseArrayCombinator = do
  _ <- symbol "array"
  len <- fromIntegral <$> natural
  ArrayCombinator len <$> parseType

parseSepByCombinator :: Parser Combinator
parseSepByCombinator = do
  _ <- symbol "sep-by"
  sep <- token stringLiteral
  SepByCombinator sep <$> parseType
