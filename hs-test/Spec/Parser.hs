{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Parser (test_testTree) where

import CCompiler.Parser qualified
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Megaparsec qualified

parserSpec :: Spec
parserSpec = do
  describe "Parser Spec" $ do
    it "symbolsParser" $
      Text.Megaparsec.parse
        CCompiler.Parser.symbolParser
        "test"
        "{"
        `shouldBe` (Right $ CCompiler.Parser.Symbol "{")

    it "keywordParser" $ do
      Text.Megaparsec.parse
        CCompiler.Parser.keywordParser
        "test"
        "int"
        `shouldBe` (Right $ CCompiler.Parser.Keyword "int")

    it "identitierParser" $ do
      Text.Megaparsec.parse
        CCompiler.Parser.identifierParser
        "test"
        "main"
        `shouldBe` (Right $ CCompiler.Parser.Identifier "main")

test_testTree :: IO TestTree
test_testTree =
  testSpec "Parser Spec" $ do
    parserSpec
