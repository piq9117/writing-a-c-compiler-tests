{-# LANGUAGE ImportQualifiedPost #-}

module Spec (main) where

import Spec.Parser qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain . testGroup "C Compiler Spec"
    =<< sequence
      [ Spec.Parser.test_testTree
      ]
