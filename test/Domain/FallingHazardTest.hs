{-# LANGUAGE OverloadedStrings #-}

-- | Pure falling hazard simulation tests.
module Domain.FallingHazardTest where

import Data.Text (Text)
import UseCases.Serialization.LevelCodec (decodeLevelText)

import Domain.Fixtures (
  dtFrame,
  floorWorld,
  testCombatParams,
 )
import Domain.Logic.BuildWorld (buildWorld)
import Domain.Logic.FallingHazards (resolveFallingHazards)
import Domain.Model.FallingHazard (
  FallingHazard (..),
  FallingHazardPhase (..),
  spawnFallingHazard,
 )
import Domain.Model.LevelDefinition (FallingHazardDef (..), LevelBuildError (..), levelFallingHazards)
import Domain.Model.Player (playerHealth, playerInvincibilityFrames, spawnPlayer)
import Domain.Model.World (World (..))
import Domain.ValueObjects.CombatParams (cpContactDamage)
import Domain.ValueObjects.Frames (frames)
import Domain.ValueObjects.Health (health, healthPoints, reduceHealth)
import Domain.ValueObjects.LifeParams (LifeParams, lifeParams)
import Domain.ValueObjects.Position (Position, posY, position)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))

testLifeParams :: LifeParams
testLifeParams = lifeParams (health 3) 64 (frames 60)

testHazard :: FallingHazard
testHazard =
  spawnFallingHazard 1 (position 0 100) 24 24 120 Nothing

fallPerFrame :: Float
fallPerFrame = 120 * 0.016

stepHazards :: World -> World
stepHazards = resolveFallingHazards testLifeParams testCombatParams dtFrame

worldWithHazard :: FallingHazard -> World -> World
worldWithHazard h w = w{worldFallingHazards = [h]}

-- | Steps until a hazard at y=100 crosses the floorWorld despawn line (y < -264).
stepsToDespawn :: Int
stepsToDespawn = 190

stepHazardsTimes :: Int -> World -> World
stepHazardsTimes n w = foldl (\w' _ -> stepHazards w') w ([1 .. n] :: [Int])

assertSingleHazard :: World -> (FallingHazard -> Assertion) -> Assertion
assertSingleHazard w assert = case worldFallingHazards w of
  [h] -> assert h
  _ -> assertFailure "expected one falling hazard"

overlapWorld :: Position -> FallingHazard -> World
overlapWorld pos hazard =
  floorWorld
    { worldPlayer = spawnPlayer (health 3) pos
    , worldFallingHazards = [hazard]
    }

stationaryOverlapHazard :: FallingHazard
stationaryOverlapHazard =
  spawnFallingHazard 1 (position 0 40) 32 32 0 Nothing

unit_hazardFallsOneFrame :: Assertion
unit_hazardFallsOneFrame =
  assertSingleHazard (stepHazards (worldWithHazard testHazard floorWorld)) $ \h -> do
    fallingHazardPhase h @?= HazardFalling
    posY (fallingHazardPos h) @?= 100 - fallPerFrame

unit_oneShotDespawnsBelowFloor :: Assertion
unit_oneShotDespawnsBelowFloor =
  let w0 = worldWithHazard testHazard floorWorld
   in worldFallingHazards (stepHazardsTimes 200 w0) @?= []

unit_loopWaitsAtSpawn :: Assertion
unit_loopWaitsAtSpawn =
  let looping =
        spawnFallingHazard 1 (position 0 100) 24 24 120 (Just (frames 30))
      wDespawned = stepHazardsTimes stepsToDespawn (worldWithHazard looping floorWorld)
   in assertSingleHazard wDespawned $ \h -> do
        fallingHazardPhase h @?= HazardWaiting (frames 30)
        posY (fallingHazardPos h) @?= 100

unit_loopFallsAgainAfterDelay :: Assertion
unit_loopFallsAgainAfterDelay =
  let looping =
        spawnFallingHazard 1 (position 0 100) 24 24 120 (Just (frames 2))
      wFallingAgain =
        stepHazardsTimes 4 $
          stepHazardsTimes stepsToDespawn (worldWithHazard looping floorWorld)
   in assertSingleHazard wFallingAgain $ \h -> do
        fallingHazardPhase h @?= HazardFalling
        posY (fallingHazardPos h) @?= 100 - fallPerFrame

unit_hazardDamagesOnDespawnFrame :: Assertion
unit_hazardDamagesOnDespawnFrame =
  let
    -- Foot y just above floorWorld despawn line (-264); one frame crosses below it.
    footY = -262.09
    hazard =
      spawnFallingHazard 1 (position 0 footY) 32 32 120 Nothing
    w' = stepHazards (overlapWorld (position 0 footY) hazard)
   in
    do
      healthPoints (playerHealth (worldPlayer w')) @?= 2
      assertBool "hazard despawned after crossing death line" $
        null (worldFallingHazards w')

unit_hazardDamagesPlayer :: Assertion
unit_hazardDamagesPlayer =
  let w' = stepHazards (overlapWorld (position 0 40) stationaryOverlapHazard)
   in healthPoints (playerHealth (worldPlayer w')) @?= 2

unit_hazardRespectsInvincibility :: Assertion
unit_hazardRespectsInvincibility =
  let damaged = reduceHealth (cpContactDamage testCombatParams) (health 3)
      w =
        (overlapWorld (position 0 40) stationaryOverlapHazard)
          { worldPlayer =
              (spawnPlayer (health 3) (position 0 40))
                { playerHealth = damaged
                , playerInvincibilityFrames = frames 10
                }
          }
      w' = stepHazards w
   in healthPoints (playerHealth (worldPlayer w')) @?= healthPoints damaged

unit_fallingHazardDefRoundTrip :: Assertion
unit_fallingHazardDefRoundTrip =
  let json :: Text
      json = "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[],\"enemies\":[],\"pickups\":[],\"fallingHazards\":[{\"id\":1,\"pos\":{\"x\":3200,\"y\":180},\"width\":24,\"height\":24,\"fallSpeed\":140,\"loopDelay\":90}],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
   in case decodeLevelText json of
        Left err -> assertFailure ("round trip decode failed: " ++ err)
        Right lvl ->
          case levelFallingHazards lvl of
            [def] -> fhDefLoopDelay def @?= Just 90
            _ -> assertFailure "expected exactly one falling hazard"

unit_buildWorldFallingHazard :: Assertion
unit_buildWorldFallingHazard =
  let json :: Text
      json = "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[{\"pos\":{\"x\":-200,\"y\":0},\"width\":400,\"height\":8}],\"movingPlatforms\":[],\"enemies\":[],\"pickups\":[],\"fallingHazards\":[{\"id\":1,\"pos\":{\"x\":0,\"y\":80},\"width\":16,\"height\":16,\"fallSpeed\":100,\"loopDelay\":60}],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
   in case decodeLevelText json of
        Left err -> assertFailure err
        Right lvl ->
          case buildWorld lvl of
            Left (LevelBuildError msg) -> assertFailure (show msg)
            Right w -> do
              length (worldFallingHazards w) @?= 1
              assertBool "hazard starts falling" $
                fallingHazardPhase (head (worldFallingHazards w)) == HazardFalling

unit_rejectInvalidFallSpeed :: Assertion
unit_rejectInvalidFallSpeed =
  let json :: Text
      json = "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[],\"enemies\":[],\"pickups\":[],\"fallingHazards\":[{\"id\":1,\"pos\":{\"x\":0,\"y\":80},\"width\":16,\"height\":16,\"fallSpeed\":0}],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
   in case decodeLevelText json of
        Left err -> assertFailure err
        Right lvl ->
          case buildWorld lvl of
            Left (LevelBuildError _) -> pure ()
            _ -> assertFailure "expected LevelBuildError for invalid fallSpeed"
