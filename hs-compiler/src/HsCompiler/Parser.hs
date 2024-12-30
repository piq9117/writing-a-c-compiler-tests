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

type Parser = Text.Megaparsec.Parsec () Text

runParser :: Text -> Maybe [Text]
runParser content = Text.Megaparsec.parseMaybe fileParser content

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

identifier :: Parser Text
identifier = do
  fmap toText $ do
    Text.Megaparsec.notFollowedBy
      ( keyword
          <|> space
          <|> openParens
          <|> closeParens
          <|> openBrace
          <|> closeBrace
          <|> semicolon
          <|> constant
      )
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
