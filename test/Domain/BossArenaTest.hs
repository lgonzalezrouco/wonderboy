{-# LANGUAGE OverloadedStrings #-}

-- | Boss arena lock: confinement and hybrid win while boss lives.
module Domain.BossArenaTest where

import Data.Text (Text)
import UseCases.Serialization.LevelCodec (decodeLevelText)

import Domain.Fixtures (
  dtFrame,
  floorWorld,
  testLifeParams,
  testParams,
 )
import Domain.Logic.BossArena (playerMayDamageEnemy)
import Domain.Logic.BuildWorld (buildWorld)
import Domain.Logic.LevelFlow (canCompleteLevel)
import Domain.Logic.Step (step)
import Domain.Model.BossArena (BossArena (..), BossArenaDef (..), mkBossArena)
import Domain.Model.Enemy (Enemy (..), enemyHealth, spawnEnemy)
import Domain.Model.EnemyKind (EnemyKind (BossGolemKind))
import Domain.Model.EntityBehaviour (waitFrames)
import Domain.Model.ExitZone (ExitZone (..))
import Domain.Model.LevelDefinition (LevelBuildError (..))
import Domain.Model.Player (
  playerPos,
  playerWidth,
  spawnPlayer,
 )
import Domain.Model.World (World (..))
import Domain.ValueObjects.Frames (frames)
import Domain.ValueObjects.Health (health)
import Domain.ValueObjects.Input (Input (..), noInput)
import Domain.ValueObjects.Position (posX, position)
import Domain.ValueObjects.Score (score)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))

testArena :: BossArena
testArena =
  BossArena
    { bossArenaLeft = 50
    , bossArenaRight = 150
    }

livingBoss :: Enemy
livingBoss =
  spawnEnemy 99 BossGolemKind (position 100 8) (waitFrames (frames 1))

arenaFloorWorld :: World
arenaFloorWorld =
  floorWorld
    { worldBossArena = Just testArena
    , worldBossArenaEngaged = False
    , worldEnemies = [livingBoss]
    }

worldInExitWithBoss :: World
worldInExitWithBoss =
  arenaFloorWorld
    { worldPlayer = spawnPlayer (health 3) (position 100 0)
    , worldMinScore = score 0
    , worldExit =
        ExitZone
          { exitPos = position 80 0
          , exitWidth = 64
          , exitHeight = 64
          }
    }

defeatedBossWorld :: World
defeatedBossWorld =
  worldInExitWithBoss
    { worldEnemies =
        [ livingBoss{enemyHealth = health 0}
        ]
    }

stepArena :: Input -> World -> World
stepArena = step testParams testLifeParams dtFrame

stepArenaTimes :: Int -> Input -> World -> World
stepArenaTimes n input w = iterate (stepArena input) w !! n

unit_hybridWinBlockedWhileBossAlive :: Assertion
unit_hybridWinBlockedWhileBossAlive =
  assertBool
    "cannot complete with living boss in exit"
    (not (canCompleteLevel (score 0) worldInExitWithBoss))

unit_hybridWinAfterBossDefeated :: Assertion
unit_hybridWinAfterBossDefeated =
  assertBool
    "can complete after boss defeated"
    (canCompleteLevel (score 0) defeatedBossWorld)

unit_bossDamageRequiresArenaEntry :: Assertion
unit_bossDamageRequiresArenaEntry = do
  let outside =
        arenaFloorWorld
          { worldPlayer = spawnPlayer (health 3) (position 40 8)
          }
      inside =
        arenaFloorWorld
          { worldPlayer = spawnPlayer (health 3) (position 100 8)
          }
  assertBool
    "boss immune to player outside arena"
    (not (playerMayDamageEnemy outside livingBoss))
  assertBool
    "boss vulnerable once player is inside arena"
    (playerMayDamageEnemy inside livingBoss)

unit_playerCanEnterArenaFromOutside :: Assertion
unit_playerCanEnterArenaFromOutside = do
  let footX = bossArenaLeft testArena + playerWidth / 2 - 20
      w0 =
        arenaFloorWorld
          { worldPlayer = spawnPlayer (health 3) (position footX 8)
          }
      w1 = stepArenaTimes 30 (noInput{inputRight = True}) w0
      minFootX = bossArenaLeft testArena + playerWidth / 2
  assertBool
    "player can walk into arena from outside while boss lives"
    (posX (playerPos (worldPlayer w1)) > minFootX + 1e-3)

unit_engagedPlayerCannotEscapeThroughJump :: Assertion
unit_engagedPlayerCannotEscapeThroughJump = do
  let minFootX = bossArenaLeft testArena + playerWidth / 2
      footX = minFootX + 4
      w0 =
        arenaFloorWorld
          { worldBossArenaEngaged = True
          , worldPlayer = spawnPlayer (health 3) (position footX 8)
          }
      jumpLeft = noInput{inputLeft = True, inputJump = True}
      w1 = stepArenaTimes 90 jumpLeft w0
  assertBool
    "engaged player cannot jump past left arena wall"
    (posX (playerPos (worldPlayer w1)) >= minFootX - 1e-3)

unit_leftWallBlocksPlayer :: Assertion
unit_leftWallBlocksPlayer = do
  let footX = bossArenaLeft testArena + playerWidth / 2 + 4
      w0 =
        arenaFloorWorld
          { worldPlayer = spawnPlayer (health 3) (position footX 8)
          }
      w1 = stepArena (noInput{inputLeft = True}) w0
      minFootX = bossArenaLeft testArena + playerWidth / 2
  assertBool
    "player stays inside left arena edge"
    (posX (playerPos (worldPlayer w1)) >= minFootX - 1e-3)

unit_rightWallBlocksPlayer :: Assertion
unit_rightWallBlocksPlayer = do
  let footX = bossArenaRight testArena - playerWidth / 2 - 4
      w0 =
        arenaFloorWorld
          { worldPlayer = spawnPlayer (health 3) (position footX 8)
          }
      w1 = stepArena (noInput{inputRight = True}) w0
      maxFootX = bossArenaRight testArena - playerWidth / 2
  assertBool
    "player stays inside right arena edge"
    (posX (playerPos (worldPlayer w1)) <= maxFootX + 1e-3)

unit_wallsInactiveAfterBossDefeated :: Assertion
unit_wallsInactiveAfterBossDefeated = do
  let footX = bossArenaRight testArena - playerWidth / 2 - 4
      maxFootX = bossArenaRight testArena - playerWidth / 2
      w0 =
        defeatedBossWorld
          { worldPlayer = spawnPlayer (health 3) (position footX 8)
          }
      w1 = stepArenaTimes 20 (noInput{inputRight = True}) w0
  assertBool
    "player passes right edge after boss defeat"
    (posX (playerPos (worldPlayer w1)) > maxFootX + 1e-3)

unit_buildWorldWithBossArena :: Assertion
unit_buildWorldWithBossArena =
  let json :: Text
      json = "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[],\"enemies\":[{\"id\":1,\"kind\":\"bossGolem\",\"pos\":{\"x\":80,\"y\":0}}],\"pickups\":[],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1},\"bossArena\":{\"left\":50,\"right\":150}}"
   in case decodeLevelText json of
        Left err -> assertFailure err
        Right lvl ->
          case buildWorld lvl of
            Left (LevelBuildError msg) -> assertFailure (show msg)
            Right w -> worldBossArena w @?= Just testArena

unit_rejectBossArenaWithoutBoss :: Assertion
unit_rejectBossArenaWithoutBoss =
  let json :: Text
      json = "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[],\"enemies\":[],\"pickups\":[],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1},\"bossArena\":{\"left\":50,\"right\":150}}"
   in case decodeLevelText json of
        Left err -> assertFailure err
        Right lvl ->
          case buildWorld lvl of
            Left (LevelBuildError _) -> pure ()
            _ -> assertFailure "expected LevelBuildError for arena without boss"

unit_rejectInvalidArenaBounds :: Assertion
unit_rejectInvalidArenaBounds =
  case mkBossArena (BossArenaDef 150 50) of
    Nothing -> pure ()
    Just _ -> assertFailure "expected invalid bounds to fail mkBossArena"
