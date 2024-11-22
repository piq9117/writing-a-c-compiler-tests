module HsCompiler.Parser where

import Text.Megaparsec qualified
import Text.Megaparsec.Char qualified

type Parser = Text.Megaparsec.Parsec () Text

keyword :: Parser Text
keyword = undefined
