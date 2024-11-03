{-# language ImportQualifiedPost #-}
{-# language OverloadedStrings #-}
module Spec.Parser (test_testTree) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import CCompiler.Parser qualified
import Text.Megaparsec qualified

parserSpec :: Spec
parserSpec = do
  describe "Parser Spec" $
    it "symbolsParser" $
      Text.Megaparsec.parse 
       CCompiler.Parser.symbolParser 
       "test"
       "{"
      `shouldBe` (Right $ CCompiler.Parser.Symbol "{")

test_testTree :: IO TestTree
test_testTree =
  testSpec "Parser Spec" $ do
    parserSpec
