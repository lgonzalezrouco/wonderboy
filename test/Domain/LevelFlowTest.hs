module Domain.LevelFlowTest where

import Domain.Logic.LevelFlow (
  bossDefeated,
  canCompleteLevel,
  hasLivingBoss,
  meetsMinScore,
  playerInExitZone,
  resolvePlayingWin,
  showBossExitHint,
  showExitScoreHint,
 )
import Domain.Model.Enemy (spawnEnemy)
import Domain.Model.EnemyKind (EnemyKind (BossGolemKind))
import Domain.Model.EntityBehaviour (waitFrames)
import Domain.Model.ExitZone (ExitZone (..))
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.Player (spawnPlayer)
import Domain.Model.World (World (..))
import Domain.ValueObjects.Frames (frames)
import Domain.ValueObjects.Health (health)
import Domain.ValueObjects.LevelCount (levelCount)
import Domain.ValueObjects.Position (position)
import Domain.ValueObjects.Score (score)
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))

exitForPlayer :: ExitZone
exitForPlayer =
  ExitZone
    { exitPos = position 0 0
    , exitWidth = 32
    , exitHeight = 64
    }

-- Player at (0,0) with default AABB overlaps exit at origin.
worldInExit :: World
worldInExit =
  World
    { worldPlayer = spawnPlayer (health 3) (position 0 0)
    , worldEnemies = []
    , worldPlatforms = []
    , worldMovingPlatforms = []
    , worldSpawnPoint = position (-100) 0
    , worldPickups = []
    , worldMinScore = score 0
    , worldExit = exitForPlayer
    , worldProjectiles = []
    , worldNextProjectileId = 1
    , worldFallingHazards = []
    }

worldAwayFromExit :: World
worldAwayFromExit =
  worldInExit{worldPlayer = spawnPlayer (health 3) (position 200 0)}

worldWithBoss :: World
worldWithBoss =
  worldInExit
    { worldEnemies =
        [ spawnEnemy 99 BossGolemKind (position 50 8) (waitFrames (frames 1))
        ]
    }

unit_playerInExitZoneWhenOverlapping :: Assertion
unit_playerInExitZoneWhenOverlapping =
  assertBool "player overlaps exit" (playerInExitZone worldInExit)

unit_playerNotInExitWhenAway :: Assertion
unit_playerNotInExitWhenAway =
  assertBool "player away from exit" (not (playerInExitZone worldAwayFromExit))

unit_canCompleteWhenExitScoreAndNoBoss :: Assertion
unit_canCompleteWhenExitScoreAndNoBoss =
  assertBool "hybrid win satisfied" (canCompleteLevel (score 0) worldInExit)

unit_cannotCompleteWithBossAlive :: Assertion
unit_cannotCompleteWithBossAlive =
  assertBool "boss blocks completion" (not (canCompleteLevel (score 0) worldWithBoss))

unit_hasLivingBossDetectsBoss :: Assertion
unit_hasLivingBossDetectsBoss =
  assertBool "boss alive" (hasLivingBoss worldWithBoss)

unit_bossDefeatedWhenNoBoss :: Assertion
unit_bossDefeatedWhenNoBoss =
  assertBool "no boss" (bossDefeated worldInExit)

unit_meetsMinScoreWhenEnough :: Assertion
unit_meetsMinScoreWhenEnough =
  assertBool
    "score ok"
    (meetsMinScore (score 100) worldInExit{worldMinScore = score 100})

unit_resolvePlayingWinLevelOne :: Assertion
unit_resolvePlayingWinLevelOne =
  resolvePlayingWin 1 (levelCount 3) (score 0) worldInExit @?= LevelComplete

unit_resolvePlayingWinLevelThree :: Assertion
unit_resolvePlayingWinLevelThree =
  resolvePlayingWin 3 (levelCount 3) (score 0) worldInExit @?= Victory

unit_resolvePlayingWinMidCatalog :: Assertion
unit_resolvePlayingWinMidCatalog =
  resolvePlayingWin 2 (levelCount 5) (score 0) worldInExit @?= LevelComplete

unit_resolvePlayingWinFinalInLongCatalog :: Assertion
unit_resolvePlayingWinFinalInLongCatalog =
  resolvePlayingWin 5 (levelCount 5) (score 0) worldInExit @?= Victory

unit_showExitScoreHintWhenLow :: Assertion
unit_showExitScoreHintWhenLow =
  let w = worldInExit{worldMinScore = score 50}
   in assertBool "score hint" (showExitScoreHint (score 10) w)

unit_showBossExitHintWhenBossAlive :: Assertion
unit_showBossExitHintWhenBossAlive =
  assertBool "boss hint" (showBossExitHint (score 0) worldWithBoss)

unit_noBossHintWhenScoreLow :: Assertion
unit_noBossHintWhenScoreLow =
  let w = worldWithBoss{worldMinScore = score 50}
   in assertBool "no boss hint before score" (not (showBossExitHint (score 10) w))
