{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module CCompiler.Parser 
  ( symbolParser, 
    Symbol(..)
  ) where

import Text.Megaparsec qualified

type Parser = Text.Megaparsec.Parsec Void Text

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
