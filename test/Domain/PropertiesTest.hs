module Domain.PropertiesTest where

import Domain.Fixtures (floorWorld, testLifeParams, testParams)
import Domain.Logic.Step (step)
import Domain.Model.Platform (platform)
import Domain.Model.Player (
  Player (..),
  playerOnGround,
  playerPos,
  spawnPlayer,
 )
import Domain.Model.World (World (..), defaultMaxHealth)
import Domain.ValueObjects.Aabb (Aabb (..), aabbOverlaps)
import Domain.ValueObjects.Damage (damage)
import Domain.ValueObjects.DeltaTime (DeltaTime, deltaTime, seconds)
import Domain.ValueObjects.Health (health, healthPoints, reduceHealth)
import Domain.ValueObjects.Input (Input (..), noInput)
import Domain.ValueObjects.Position (posY, position)
import Domain.ValueObjects.Score (score, scorePoints)
import Domain.ValueObjects.Velocity (velocity)
import Test.Tasty.QuickCheck (
  Gen,
  Property,
  arbitrary,
  choose,
  counterexample,
  forAll,
  property,
  (===),
 )

genInput :: Gen Input
genInput =
  Input <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

genIdentityWorld :: Gen World
genIdentityWorld = do
  x <- choose (-400, 400)
  y <- choose (-50, 300)
  vx <- choose (-600, 600)
  vy <- choose (-600, 600)
  onGround <- arbitrary
  let p =
        (spawnPlayer defaultMaxHealth (position x y))
          { playerVel = velocity vx vy
          , playerOnGround = onGround
          }
  pure floorWorld{worldPlayer = p}

genAabb :: Gen Aabb
genAabb = do
  x <- choose (-200, 200)
  y <- choose (-200, 200)
  w <- choose (0, 120)
  h <- choose (0, 120)
  pure (Aabb x y (x + w) (y + h))

genTunnel :: Gen (DeltaTime, Float, Float, Float)
genTunnel = do
  h <- choose (4, 12)
  dtSec <- choose (0.008, 0.033)
  k <- choose (1.5, 6.0)
  gap <- choose (0.5, 3.0)
  let dy = k * h
      vy = negate (dy / dtSec)
  pure (deltaTime dtSec, vy, h, gap)

prop_stepZeroIsIdentity :: Property
prop_stepZeroIsIdentity =
  forAll genIdentityWorld $ \w ->
    forAll genInput $ \i ->
      step testParams testLifeParams (deltaTime 0) i w === w

prop_aabbOverlapsSymmetric :: Property
prop_aabbOverlapsSymmetric =
  forAll genAabb $ \a ->
    forAll genAabb $ \b ->
      aabbOverlaps a b === aabbOverlaps b a

prop_aabbOverlapsReflexive :: Property
prop_aabbOverlapsReflexive =
  forAll genAabb $ \a -> property (aabbOverlaps a a)

prop_substepPreventsTunneling :: Property
prop_substepPreventsTunneling =
  forAll genTunnel $ \(dt, vy, h, gap) ->
    let plat = platform (position (-200) 0) 400 h
        faller =
          (spawnPlayer defaultMaxHealth (position 0 (h + gap)))
            { playerVel = velocity 0 vy
            , playerOnGround = False
            }
        w0 =
          floorWorld
            { worldPlayer = faller
            , worldPlatforms = [plat]
            , worldMovingPlatforms = []
            }
        p1 = worldPlayer (step testParams testLifeParams dt noInput w0)
        platTop = h
     in counterexample
          ("foot=" ++ show (posY (playerPos p1)) ++ " platTop=" ++ show platTop)
          (posY (playerPos p1) >= platTop - 1e-2 && playerOnGround p1)

prop_deltaTimeNeverNegative :: Property
prop_deltaTimeNeverNegative =
  forAll (choose (-100, 100)) $ \x -> seconds (deltaTime x) === max 0 x

prop_scoreNeverNegative :: Property
prop_scoreNeverNegative =
  forAll (choose (-1000, 1000)) $ \n -> scorePoints (score n) === max 0 n

prop_healthNeverNegative :: Property
prop_healthNeverNegative =
  forAll (choose (-100, 100)) $ \n -> healthPoints (health n) === max 0 n

prop_reduceHealthNeverNegative :: Property
prop_reduceHealthNeverNegative =
  forAll (choose (0, 100)) $ \n ->
    forAll (choose (0, 100)) $ \d ->
      healthPoints (reduceHealth (damage d) (health n)) >= 0
