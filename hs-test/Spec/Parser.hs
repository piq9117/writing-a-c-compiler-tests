module Spec.Parser (test_testTree) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

parserSpec :: Spec
parserSpec = do
  describe "Parser Spec" $
    it "blah" $
      1 `shouldBe` 1

test_testTree :: IO TestTree
test_testTree =
  testSpec "Parser Spec" $ do
    parserSpec
