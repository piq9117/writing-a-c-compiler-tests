{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Parser
  ( test_testTree,
  )
where

import HsCompiler.Parser qualified
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Megaparsec qualified
import Text.Megaparsec.Char qualified

parser :: Spec
parser = describe "Parsers" $ do
  it "keywords" $
    Text.Megaparsec.parse
      (many (HsCompiler.Parser.keyword <* Text.Megaparsec.Char.eol))
      "test"
      (unlines HsCompiler.Parser.keywords)
      `shouldBe` (Right ["int", "void", "return"])

  it "identifier" $ do
    Text.Megaparsec.parse
      HsCompiler.Parser.identifier
      "test"
      "deeznuts"
      `shouldBe` (Right "deeznuts")

    Text.Megaparsec.parse
      HsCompiler.Parser.identifier
      "test"
      "deez_nuts"
      `shouldBe` (Right "deez_nuts")

    Text.Megaparsec.parse
      HsCompiler.Parser.identifier
      -- ((fmap toText $ many Text.Megaparsec.Char.asciiChar) :: HsCompiler.Parser.Parser Text)
      "test"
      "deezNuts"
      `shouldBe` (Right "deezNuts")

    Text.Megaparsec.parse
      HsCompiler.Parser.identifier
      "test"
      "1"
      `shouldBe` (Right "deezNuts")

test_testTree :: IO TestTree
test_testTree =
  testSpec "Parser Spec" $ do
    parser
