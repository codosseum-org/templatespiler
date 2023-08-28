module Language.Templatespiler.Parser where

import Data.Text (pack)
import Language.Templatespiler.Syntax
import Text.Trifecta
import Prelude hiding (Type)

parseBindingList :: Parser BindingList
parseBindingList = BindingList . fromList <$> some parseBinding

parseBinding :: Parser Binding
parseBinding = do
  name <- token parseIdent
  _ <- symbol ":"
  Binding name <$> parseType

parseIdent :: Parser Ident
parseIdent = Ident . toText <$> some letter

parseType :: Parser Type
parseType = (TerminalType <$> parseTerminalType) <|> parseCombinatorType

parseTerminalType :: Parser TerminalType
parseTerminalType =
  IntType <$ symbol "Integer"

parseCombinatorType :: Parser Type
parseCombinatorType = CombinatorType <$> parseCombinator

parseCombinator :: Parser Combinator
parseCombinator =
  parseGroupCombinator
    <|> parens parseCombinator
    <|> parseArrayCombinator
    <|> parseSepByCombinator
    <|> parseListCombinator

-- <|> parseNamedCombinator

parseNamedCombinator :: Parser Combinator
parseNamedCombinator = do
  name <- parseIdent
  _ <- symbol ":"
  NamedCombinator name <$> parseCombinator

parseListCombinator :: Parser Combinator
parseListCombinator = do
  _ <- symbol "list"
  ListCombinator <$> parseCombinator

parseGroupCombinator :: Parser Combinator
parseGroupCombinator = do
  _ <- symbol "["
  GroupCombinator <$> parseBindingList

parseArrayCombinator :: Parser Combinator
parseArrayCombinator = do
  _ <- symbol "array"
  len <- fromIntegral <$> natural
  ArrayCombinator len <$> parseCombinator

parseSepByCombinator :: Parser Combinator
parseSepByCombinator = do
  _ <- symbol "sepBy"
  sep <- stringLiteral

  SepByCombinator sep <$> parseCombinator
