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
  it "keyword" $
    Text.Megaparsec.parse
      (many (HsCompiler.Parser.keyword <* Text.Megaparsec.Char.eol))
      "test"
      (unlines HsCompiler.Parser.keywords)
      `shouldBe` (Right ["int", "void", "return"])

test_testTree :: IO TestTree
test_testTree =
  testSpec "Parser Spec" $ do
    parser
