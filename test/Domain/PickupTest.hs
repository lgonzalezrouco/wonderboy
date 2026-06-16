-- | Pure pickup overlap and collection tests.
module Domain.PickupTest where

import Domain.Fixtures (mkTestPickup, worldWithPickups)
import Domain.Logic.Pickups (resolvePickups)
import Domain.Model.Pickup (Pickup, mkPickup)
import Domain.Model.World (worldPickups)
import Domain.ValueObjects.Position (Position, position)
import Test.Tasty.HUnit (Assertion, (@?=))

overlapPos :: Position
overlapPos = position 0 8

adjacentPlayerPos :: Position
adjacentPlayerPos = position 50 8

resolveAt :: Position -> [Pickup] -> ([Pickup], Int)
resolveAt playerPos pickups =
  let (w', scoreDelta) = resolvePickups (worldWithPickups playerPos pickups)
   in (worldPickups w', scoreDelta)

unit_overlapCollectsPickup :: Assertion
unit_overlapCollectsPickup =
  let pickup = mkTestPickup 1 overlapPos 100
      (remaining, delta) = resolveAt overlapPos [pickup]
   in do
        remaining @?= []
        delta @?= 100

unit_noOverlapLeavesPickup :: Assertion
unit_noOverlapLeavesPickup =
  let pickup = mkTestPickup 1 overlapPos 100
      (remaining, delta) = resolveAt adjacentPlayerPos [pickup]
   in do
        remaining @?= [pickup]
        delta @?= 0

unit_multiplePickupsSameFrame :: Assertion
unit_multiplePickupsSameFrame =
  let p1 = mkTestPickup 1 (position (-5) 8) 100
      p2 = mkTestPickup 2 (position 5 8) 50
      (remaining, delta) = resolveAt overlapPos [p1, p2]
   in do
        remaining @?= []
        delta @?= 150

unit_partialOverlapCollectsOne :: Assertion
unit_partialOverlapCollectsOne =
  let near = mkTestPickup 1 overlapPos 75
      far = mkTestPickup 2 adjacentPlayerPos 25
      (remaining, delta) = resolveAt overlapPos [near, far]
   in do
        remaining @?= [far]
        delta @?= 75

unit_mkPickupRejectsNegative :: Assertion
unit_mkPickupRejectsNegative =
  mkPickup 1 overlapPos (-1) @?= Nothing

unit_zeroValuePickupCollects :: Assertion
unit_zeroValuePickupCollects =
  let pickup = mkTestPickup 1 overlapPos 0
      (remaining, delta) = resolveAt overlapPos [pickup]
   in do
        remaining @?= []
        delta @?= 0
