{-# LANGUAGE OverloadedStrings #-}

module HsCompiler.Parser
  ( Parser,
    keyword,
    keywords,
    identifier,
    openParens,
    closeParens,
    openBrace,
    closeBrace,
    semicolon,
    constant,
    runParser,
    fileParser,
    space,
  )
where

import Control.Applicative.Combinators (choice)
import Data.Char qualified
import Data.Text qualified
import Text.Megaparsec qualified
import Text.Megaparsec.Char qualified

type Parser = Text.Megaparsec.Parsec Void Text

runParser :: Text -> Either (Text.Megaparsec.ParseErrorBundle Text Void) [Text]
runParser content = Text.Megaparsec.parse fileParser "" content

fileParser :: Parser [Text]
fileParser =
  many $
    keyword
      <|> space
      <|> identifier
      <|> openParens
      <|> closeParens
      <|> openBrace
      <|> closeBrace
      <|> semicolon
      <|> constant

keyword :: Parser Text
keyword =
  ( choice $
      fmap
        (Text.Megaparsec.try <<< Text.Megaparsec.Char.string)
        keywords
  )

keywords :: [Text]
keywords = ["int", "void", "return"]

invalidIdentifiers :: [Text]
invalidIdentifiers = ["@"]

illegalIdentifier :: Parser Text
illegalIdentifier = do
  choice $
    fmap
      Text.Megaparsec.Char.string
      invalidIdentifiers

invalidStart :: Parser ()
invalidStart = do
  ident <- Text.Megaparsec.optional illegalIdentifier
  case ident of
    Nothing -> pure ()
    Just illegalChar -> fail $ "Illegal character found: " <> (toString illegalChar)

identifier :: Parser Text
identifier = do
  fmap toText $ do
    Text.Megaparsec.notFollowedBy
      ( keyword
          <|> openParens
          <|> closeParens
          <|> openBrace
          <|> closeBrace
          <|> semicolon
          <|> constant
      )
    void invalidStart
    start <- Text.Megaparsec.satisfy (not <<< Data.Char.isDigit)
    rest <- many identifierRest
    pure (start : rest)
  where
    identifierRest =
      do
        -- TODO asciiChar causes to parse parenthesis
        -- Text.Megaparsec.Char.asciiChar
        Text.Megaparsec.Char.upperChar
        <|> Text.Megaparsec.Char.lowerChar
        <|> Text.Megaparsec.Char.char '_'

openParens :: Parser Text
openParens =
  toText <$> Text.Megaparsec.Char.string "("

closeParens :: Parser Text
closeParens =
  toText <$> Text.Megaparsec.Char.string ")"

openBrace :: Parser Text
openBrace =
  toText <$> (Text.Megaparsec.Char.string "{")

closeBrace :: Parser Text
closeBrace =
  toText <$> (Text.Megaparsec.Char.string "}")

semicolon :: Parser Text
semicolon =
  toText <$> (Text.Megaparsec.Char.string ";")

constant :: Parser Text
constant = do
  constantChar <- toText <$> many Text.Megaparsec.Char.digitChar
  if Data.Text.null constantChar
    then fail "not a constant character"
    else pure constantChar

space :: Parser Text
space = do
  spaceChar <- Text.Megaparsec.Char.spaceChar
  pure (toText [spaceChar])
