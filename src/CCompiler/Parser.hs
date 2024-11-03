{-# LANGUAGE OverloadedStrings #-}

module CCompiler.Parser where

import Text.Megaparsec qualified

type Parser = Text.Megaparsec.Parsec Void Text

newtype Symbol = Symbol Text

symbolParser :: Parser Symbol
symbolParser = do
  symbol <- symbols
  pure (Symbol $ toText [symbol])

symbols :: Parser Char
symbols =
  Text.Megaparsec.oneOf
    ("{}();" :: [Text.Megaparsec.Token Text])
