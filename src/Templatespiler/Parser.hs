module Templatespiler.Parser where

import Data.Char (isAlphaNum)
import Templatespiler.AST
import Text.Megaparsec
  ( MonadParsec (..),
    Parsec,
    sepBy1,
    sepEndBy1,
  )
import Text.Megaparsec.Char (char, newline, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug (dbg)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

scn :: Parser ()
scn = L.space space1 empty empty

eol :: Parser ()
eol = optional (keychar ';') *> void (many newline)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

keyword :: Text -> Parser ()
keyword = void . lexeme . string

keychar :: Char -> Parser ()
keychar = void . lexeme . char

parseBlock :: Parser Block
parseBlock =
  fromList
    <$> sepEndBy1 (lexeme parseStatement) eol

parseStatement :: Parser Statement
parseStatement = sc *> (parseYield <|> parsePrintLn <|> parseVariableDecl)

parseYield :: Parser Statement
parseYield = do
  keyword "yield"
  Yield <$> parseBinder

parseVariableDecl :: Parser Statement
parseVariableDecl = do
  binder <- parseBinder
  keychar '='
  VariableDecl binder <$> parseExpression

parsePrintLn :: Parser Statement
parsePrintLn = do
  keyword "println"
  PrintLn <$> parseStringLit

parseBinder :: Parser Binder
parseBinder =
  Binder . fromList <$> parseIdentifier `sepBy1` keychar ','

parseExpression :: Parser Expression
parseExpression = parseReadLn <|> parseFor

parseReadLn :: Parser Expression
parseReadLn = do
  keyword "readln"
  ReadLn <$> parseReadType

parseReadType :: Parser ReadType
parseReadType =
  let readTypeAtom =
        (keyword "Integer" $> ReadInteger)
          <|> (keyword "Long" $> ReadLong)
          <|> (keyword "String" $> ReadString)
   in ReadWords . fromList <$> (readTypeAtom `sepBy1` sc) <|> readTypeAtom

parseFor :: Parser Expression
parseFor = do
  keyword "for"
  count <- parseIdentifier
  keyword "as"
  var <- parseIdentifier

  keychar '{'
  optional newline
  block <- parseBlock
  keychar '}'
  return $ For count var block

parseStringLit :: Parser Text
parseStringLit = lexeme $ char '"' *> takeWhileP (Just "string literal") (/= '"') <* char '"'

parseIdentifier :: Parser Text
parseIdentifier = lexeme $ takeWhile1P (Just "identifier") isAlphaNum