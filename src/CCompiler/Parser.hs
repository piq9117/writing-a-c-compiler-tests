{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module CCompiler.Parser
  ( Symbol (..),
    Keyword (..),
    symbolParser,
    keywordParser,
  )
where

import Control.Applicative.Combinators (choice)
import Text.Megaparsec qualified
import Text.Megaparsec.Char qualified

type Parser = Text.Megaparsec.Parsec Void Text

newtype Keyword = Keyword Text
  deriving newtype (Eq, Show)

keywordParser :: Parser Keyword
keywordParser = do
  keyword <- keywords
  pure (Keyword keyword)

newtype Symbol = Symbol Text
  deriving newtype (Eq, Show)

symbolParser :: Parser Symbol
symbolParser = do
  symbol <- symbols
  pure (Symbol $ toText [symbol])

symbols :: Parser Char
symbols =
  Text.Megaparsec.oneOf
    ("{}();" :: [Text.Megaparsec.Token Text])

keywords :: Parser Text
keywords =
  choice $
    fmap
      Text.Megaparsec.Char.string
      [ "int",
        "void"
      ]
