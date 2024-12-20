{-# LANGUAGE OverloadedStrings #-}

module HsCompiler.Parser
  ( keyword,
    keywords,
    identifier,
    openParens,
    closeParens,
    openBrace,
    closeBrace,
    semicolon,
    constant,
    Parser,
  )
where

import Control.Applicative.Combinators (choice)
import Text.Megaparsec qualified
import Text.Megaparsec.Char qualified

type Parser = Text.Megaparsec.Parsec () Text

keyword :: Parser Text
keyword =
  choice $
    fmap
      (Text.Megaparsec.try <<< Text.Megaparsec.Char.string)
      keywords

keywords :: [Text]
keywords = ["int", "void", "return"]

identifier :: Parser Text
identifier =
  fmap toText $
    many identifierStart
  where
    -- TODO this will output null
    -- if the identifier starts with a digit
    identifierStart = do
      Text.Megaparsec.notFollowedBy Text.Megaparsec.Char.digitChar
      Text.Megaparsec.Char.asciiChar
        <|> Text.Megaparsec.Char.upperChar
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
constant = toText <$> many Text.Megaparsec.Char.digitChar
