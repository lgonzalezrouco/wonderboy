module Main (main) where

import Domain.AabbTest qualified as AabbTest
import Domain.StepTest qualified as StepTest
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "wonderboy-hs"
      [ StepTest.tests
      , AabbTest.tests
      ]
