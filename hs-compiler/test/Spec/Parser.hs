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
      "main(void)"
      `shouldBe` (Right "main")

    Text.Megaparsec.parse
      HsCompiler.Parser.identifier
      "test"
      "deez_nuts"
      `shouldBe` (Right "deez_nuts")

    Text.Megaparsec.parse
      HsCompiler.Parser.identifier
      "test"
      "deezNuts"
      `shouldBe` (Right "deezNuts")

    let result =
          Text.Megaparsec.parse
            HsCompiler.Parser.identifier
            "test"
            "1DeezNuts"

    (isLeft result) `shouldBe` True

  it "constant" $ do
    let result =
          Text.Megaparsec.parse
            HsCompiler.Parser.constant
            "test"
            "int"

    (isLeft result) `shouldBe` True

  it "space" $ do
    Text.Megaparsec.parse
      HsCompiler.Parser.space
      "test"
      " "
      `shouldBe` (Right " ")

  it "fileParser" $ do
    Text.Megaparsec.parse
      HsCompiler.Parser.fileParser
      "test"
      "int main(void){return 0;}"
      `shouldBe` (Right [])

test_testTree :: IO TestTree
test_testTree =
  testSpec "Parser Spec" $ do
    parser
