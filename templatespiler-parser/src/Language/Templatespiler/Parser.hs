{-# LANGUAGE OverloadedLists #-}

module Language.Templatespiler.Parser where

import Data.Char (isLower)
import Data.Text (pack)
import Language.Templatespiler.Syntax
import Text.Parser.Token.Highlight (Highlight (..))
import Text.Trifecta
import Prelude hiding (Type)

lowerLetter :: (CharParsing m) => m Char
lowerLetter = satisfy isLower <?> "lowercase letter"

identifierStyle :: (CharParsing m) => IdentifierStyle m
identifierStyle =
  IdentifierStyle
    { _styleName = "lowercase identifier"
    , _styleStart = lowerLetter
    , _styleLetter = alphaNum <|> char '_'
    , _styleReserved = ["list", "array", "sep-by"]
    , _styleHighlight = Identifier
    , _styleReservedHighlight = ReservedIdentifier
    }

parseBindingList :: Parser BindingList
parseBindingList = BindingList . fromList <$> some parseBinding

parseBinding :: Parser Binding
parseBinding = do
  name <- token parseIdent
  _ <- symbolic ':'
  Binding name <$> parseType

parseIdent :: Parser Ident
parseIdent = Ident <$> ident identifierStyle

parseType :: Parser Type
parseType =
  (TerminalType <$> try parseTerminalType)
    <|> parseCombinatorType

parseTerminalType :: Parser TerminalType
parseTerminalType =
  IntType <$ symbol "Integer"
    <|> FloatType <$ symbol "Float"
    <|> StringType <$ symbol "String"

parseCombinatorType :: Parser Type
parseCombinatorType = CombinatorType <$> parseCombinator <?> "combinator"

parseCombinator :: Parser Combinator
parseCombinator =
  (parseGroupCombinator <?> "group combinator")
    <|> (parens parseCombinator <?> "combinator in parens")
    <|> (parseArrayCombinator <?> "array combinator")
    <|> (parseSepByCombinator <?> "combinator")
    <|> (parseListCombinator <?> "list combinator")
    <|> (parseNamedCombinator <?> "named combinator")

parseNamedCombinator :: Parser Combinator
parseNamedCombinator = do
  name <- token parseIdent
  _ <- symbol ":"
  NamedCombinator name <$> parseType

parseListCombinator :: Parser Combinator
parseListCombinator = do
  _ <- reserve identifierStyle "list"
  ListCombinator <$> parseType

parseGroupCombinator :: Parser Combinator
parseGroupCombinator = do
  _ <- symbol "["
  l <- parseBindingList
  _ <- symbol "]"
  pure $ GroupCombinator l

parseArrayCombinator :: Parser Combinator
parseArrayCombinator = do
  _ <- reserve identifierStyle "array"
  len <- fromIntegral <$> natural
  ArrayCombinator len <$> parseType

parseSepByCombinator :: Parser Combinator
parseSepByCombinator = do
  _ <- reserve identifierStyle "sep-by"
  sep <- token stringLiteral
  SepByCombinator sep <$> parseType
