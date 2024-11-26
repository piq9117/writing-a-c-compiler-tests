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

parser :: Spec
parser = describe "Parsers" $ do
  it "keyword" $
    do
      (Text.Megaparsec.parse HsCompiler.Parser.keyword "test" "int")
      `shouldBe` (Right "int")

test_testTree :: IO TestTree
test_testTree =
  testSpec "Parser Spec" $ do
    parser
