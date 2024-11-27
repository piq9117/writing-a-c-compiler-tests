{-# LANGUAGE OverloadedStrings #-}

module HsCompiler.Parser
  ( keyword,
    keywords,
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
