module Domain.AabbTest (tests) where

import Domain.ValueObjects.Aabb (Aabb (..), aabbOverlaps)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Domain.ValueObjects.Aabb"
    [ testCase "collisionSymmetric" testCollisionSymmetric
    ]

boxA :: Aabb
boxA = Aabb 0 0 10 10

boxB :: Aabb
boxB = Aabb 5 5 15 15

boxC :: Aabb
boxC = Aabb 20 20 30 30

testCollisionSymmetric :: Assertion
testCollisionSymmetric = do
  aabbOverlaps boxA boxB @?= aabbOverlaps boxB boxA
  aabbOverlaps boxA boxC @?= aabbOverlaps boxC boxA
  aabbOverlaps boxB boxC @?= aabbOverlaps boxC boxB
