{-# LANGUAGE OverloadedStrings #-}

-- | Pure crumbling platform simulation tests.
module Domain.CrumblingPlatformTest where

import Data.Text (Text)
import UseCases.Serialization.LevelCodec (decodeLevelText)

import Domain.Fixtures (
  dtFrame,
  floorWorld,
  testLifeParams,
  testParams,
 )
import Domain.Logic.BehaviourCatalog (defaultProgramForKind)
import Domain.Logic.BuildWorld (buildWorld)
import Domain.Logic.Step (step)
import Domain.Model.CrumblingPlatform (
  CrumblingPlatform (..),
  CrumblingPlatformPhase (..),
  crumbleCountdownFrames,
  crumbleFallSpeed,
  spawnCrumblingPlatform,
 )
import Domain.Model.Enemy (spawnEnemy)
import Domain.Model.EnemyKind (EnemyKind (SnailKind))
import Domain.Model.LevelDefinition (CrumblingPlatformDef (..), LevelBuildError (..), levelCrumblingPlatforms)
import Domain.Model.Player (
  Player (..),
  playerOnGround,
  spawnPlayer,
 )
import Domain.Model.World (World (..))
import Domain.ValueObjects.Frames (frameCount)
import Domain.ValueObjects.Health (health)
import Domain.ValueObjects.Input (noInput)
import Domain.ValueObjects.Position (posY, position)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))

fallPerFrame :: Float
fallPerFrame = crumbleFallSpeed * 0.016

testCrumbling :: CrumblingPlatform
testCrumbling =
  spawnCrumblingPlatform 1 (position 0 0) 64 8

-- | Pies sobre el tramo superior (top y = 8).
onCrumblingTop :: Player
onCrumblingTop =
  (spawnPlayer (health 3) (position 0 8)){playerOnGround = True}

worldOnCrumbling :: World
worldOnCrumbling =
  floorWorld
    { worldPlatforms = []
    , worldCrumblingPlatforms = [testCrumbling]
    , worldPlayer = onCrumblingTop
    }

stepWorld :: World -> World
stepWorld = step testParams testLifeParams dtFrame noInput

stepWorldTimes :: Int -> World -> World
stepWorldTimes n w = foldl (\w' _ -> stepWorld w') w ([1 .. n] :: [Int])

assertSingleCrumbling :: World -> (CrumblingPlatform -> Assertion) -> Assertion
assertSingleCrumbling w assert = case worldCrumblingPlatforms w of
  [cp] -> assert cp
  _ -> assertFailure "expected one crumbling platform"

unit_spawnCrumblingPlatformIntact :: Assertion
unit_spawnCrumblingPlatformIntact =
  crumblingPlatformPhase testCrumbling @?= CrumbleIntact

unit_standingOnCrumblingStartsCountdown :: Assertion
unit_standingOnCrumblingStartsCountdown =
  assertSingleCrumbling (stepWorld worldOnCrumbling) $ \cp -> do
    case crumblingPlatformPhase cp of
      CrumbleCountingDown remaining ->
        frameCount remaining @?= frameCount crumbleCountdownFrames - 1
      other -> assertFailure ("expected CrumbleCountingDown, got " ++ show other)

unit_playerSolidDuringCountdown :: Assertion
unit_playerSolidDuringCountdown =
  let w = stepWorldTimes (frameCount crumbleCountdownFrames `div` 2) worldOnCrumbling
   in assertBool "player on ground mid-countdown" $
        playerOnGround (worldPlayer w)

unit_playerFallsThroughAfterCrumble :: Assertion
unit_playerFallsThroughAfterCrumble =
  let w = stepWorldTimes (frameCount crumbleCountdownFrames + 1) worldOnCrumbling
   in assertBool "player no longer on ground after crumble" $
        not (playerOnGround (worldPlayer w))

unit_crumblingFallsAndDespawns :: Assertion
unit_crumblingFallsAndDespawns =
  let wAfterCountdown = stepWorldTimes (frameCount crumbleCountdownFrames) worldOnCrumbling
      wDespawned = stepWorldTimes 200 wAfterCountdown
   in do
        assertSingleCrumbling wAfterCountdown $ \cp ->
          crumblingPlatformPhase cp @?= CrumbleFalling
        worldCrumblingPlatforms wDespawned @?= []

unit_enemyDoesNotTriggerCrumble :: Assertion
unit_enemyDoesNotTriggerCrumble =
  let enemy =
        spawnEnemy 1 SnailKind (position 0 8) (defaultProgramForKind SnailKind)
      w =
        worldOnCrumbling
          { worldPlayer = spawnPlayer (health 3) (position 200 80)
          , worldEnemies = [enemy]
          }
      w' = stepWorld w
   in assertSingleCrumbling w' $ \cp ->
        crumblingPlatformPhase cp @?= CrumbleIntact

unit_crumblingPlatformDefRoundTrip :: Assertion
unit_crumblingPlatformDefRoundTrip =
  let json :: Text
      json = "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[],\"enemies\":[],\"pickups\":[],\"crumblingPlatforms\":[{\"id\":1,\"pos\":{\"x\":620,\"y\":120},\"width\":48,\"height\":8}],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
   in case decodeLevelText json of
        Left err -> assertFailure ("round trip decode failed: " ++ err)
        Right lvl ->
          case levelCrumblingPlatforms lvl of
            [def] -> cpDefWidth def @?= 48
            _ -> assertFailure "expected exactly one crumbling platform"

unit_buildWorldCrumblingPlatform :: Assertion
unit_buildWorldCrumblingPlatform =
  let json :: Text
      json = "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[{\"pos\":{\"x\":-200,\"y\":0},\"width\":400,\"height\":8}],\"movingPlatforms\":[],\"enemies\":[],\"pickups\":[],\"crumblingPlatforms\":[{\"id\":1,\"pos\":{\"x\":0,\"y\":80},\"width\":48,\"height\":8}],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
   in case decodeLevelText json of
        Left err -> assertFailure err
        Right lvl ->
          case buildWorld lvl of
            Left (LevelBuildError msg) -> assertFailure (show msg)
            Right w -> do
              length (worldCrumblingPlatforms w) @?= 1
              crumblingPlatformPhase (head (worldCrumblingPlatforms w))
                @?= CrumbleIntact

unit_rejectInvalidCrumblingWidth :: Assertion
unit_rejectInvalidCrumblingWidth =
  let json :: Text
      json = "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[],\"enemies\":[],\"pickups\":[],\"crumblingPlatforms\":[{\"id\":1,\"pos\":{\"x\":0,\"y\":80},\"width\":0,\"height\":8}],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
   in case decodeLevelText json of
        Left err -> assertFailure err
        Right lvl ->
          case buildWorld lvl of
            Left (LevelBuildError _) -> pure ()
            _ -> assertFailure "expected LevelBuildError for invalid width"

unit_fallingCrumblingMovesDown :: Assertion
unit_fallingCrumblingMovesDown =
  let w0 = stepWorldTimes (frameCount crumbleCountdownFrames) worldOnCrumbling
      w1 = stepWorld w0
   in assertSingleCrumbling w1 $ \cp -> do
        crumblingPlatformPhase cp @?= CrumbleFalling
        posY (crumblingPlatformPos cp) @?= 0 - fallPerFrame
