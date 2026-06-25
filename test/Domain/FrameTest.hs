-- | Composition tests for 'Domain.Logic.Frame.advanceSimulationFrame'.
module Domain.FrameTest where

import Data.List (find)

import Domain.Fixtures (
  demoWorld,
  dtFrame,
  floorWorld,
  mkTestPickup,
  testCombatParams,
  testLifeParams,
  testParams,
  testThrowParams,
 )
import Domain.Logic.BossCatalog (bossDefinitionForKind)
import Domain.Logic.Frame (
  FrameParams (..),
  FrameResult (..),
  PlayingFrame (..),
  advanceSimulationFrame,
 )
import Domain.Model.BossPhase (
  bossMaxHealth,
  bossPhaseNumber,
  bossPhases,
  phaseProgram,
 )
import Domain.Model.Enemy (
  Enemy (..),
  enemyBossPhase,
  enemyHealth,
  enemyKind,
  spawnEnemy,
 )
import Domain.Model.EnemyKind (EnemyKind (..), isBossKind)
import Domain.Model.ExitZone (ExitZone (..))
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.Player (Player (..), spawnPlayer)
import Domain.Model.World (World (..), defaultMaxHealth)
import Domain.ValueObjects.Health (health)
import Domain.ValueObjects.Input (Input (..), noInput)
import Domain.ValueObjects.Position (Position, position)
import Domain.ValueObjects.Score (score)
import Test.Tasty.HUnit (Assertion, (@?=))
import UseCases.GameMonad (defaultConfig, gcLevelCount, gcStartingLives)

testFrameParams :: FrameParams
testFrameParams =
  FrameParams
    { fpPhysics = testParams
    , fpLife = testLifeParams
    , fpCombat = testCombatParams
    , fpThrow = testThrowParams
    }

playingFrame :: World -> PlayingFrame
playingFrame w =
  PlayingFrame
    { pfWorld = w
    , pfLives = gcStartingLives defaultConfig
    , pfScore = score 0
    , pfLevelIndex = 1
    }

runFrame :: World -> FrameResult
runFrame w =
  advanceSimulationFrame
    testFrameParams
    (gcLevelCount defaultConfig)
    dtFrame
    noInput
    (playingFrame w)

spawnBossFromCatalog :: Int -> EnemyKind -> Position -> Enemy
spawnBossFromCatalog eid kind pos =
  case bossDefinitionForKind kind of
    Nothing -> error "spawnBossFromCatalog: missing catalog"
    Just def ->
      case bossPhases def of
        (phase0 : _) ->
          let e = spawnEnemy eid kind pos (phaseProgram phase0)
              maxHp = bossMaxHealth def
           in e{enemyHealth = maxHp, enemyMaxHealth = maxHp}
        [] -> error "spawnBossFromCatalog: empty phases"

golemKingAt :: Enemy
golemKingAt = spawnBossFromCatalog 1 BossGolemKind (position 170 8)

bossPhaseIndexIn :: World -> Int
bossPhaseIndexIn w =
  case find (isBossKind . enemyKind) (worldEnemies w) of
    Nothing -> -1
    Just e -> maybe (-1) bossPhaseNumber (enemyBossPhase e)

unit_frameHybridWinSetsLevelComplete :: Assertion
unit_frameHybridWinSetsLevelComplete =
  let exitZone =
        ExitZone
          { exitPos = position 0 0
          , exitWidth = 32
          , exitHeight = 64
          }
      w =
        demoWorld
          { worldPlayer = spawnPlayer defaultMaxHealth (position 0 0)
          , worldMinScore = score 0
          , worldExit = exitZone
          , worldEnemies = []
          }
   in frPhase (runFrame w) @?= LevelComplete

unit_frameDeathOverridesHybridWin :: Assertion
unit_frameDeathOverridesHybridWin =
  let exitZone =
        ExitZone
          { exitPos = position 0 0
          , exitWidth = 32
          , exitHeight = 64
          }
      w =
        demoWorld
          { worldPlayer =
              (spawnPlayer defaultMaxHealth (position 0 0))
                { playerHealth = health 0
                }
          , worldMinScore = score 0
          , worldExit = exitZone
          , worldEnemies = []
          }
      result = runFrame w
   in frPhase result @?= Playing

unit_framePickupScoreCountsForHybridWin :: Assertion
unit_framePickupScoreCountsForHybridWin =
  let exitZone =
        ExitZone
          { exitPos = position 0 0
          , exitWidth = 32
          , exitHeight = 64
          }
      pickup = mkTestPickup 1 (position 0 0) 100
      w =
        floorWorld
          { worldPlayer = spawnPlayer defaultMaxHealth (position 0 0)
          , worldMinScore = score 100
          , worldExit = exitZone
          , worldPickups = [pickup]
          , worldEnemies = []
          }
      result = runFrame w
   in do
        frPhase result @?= LevelComplete
        frScore result @?= score 100

unit_frameBossPhaseUsesPreFrameHealth :: Assertion
unit_frameBossPhaseUsesPreFrameHealth =
  let boss =
        golemKingAt
          { enemyHealth = health 14
          , enemyMaxHealth = health 20
          }
      -- Arranca el swing por input: el melee daña en el frame de inicio ('attackStarted').
      p = spawnPlayer defaultMaxHealth (position 170 8) -- mira a la derecha por defecto
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [boss]
          }
      result =
        advanceSimulationFrame
          testFrameParams
          (gcLevelCount defaultConfig)
          dtFrame
          (noInput{inputAttack = True})
          (playingFrame w)
   in bossPhaseIndexIn (frWorld result) @?= 1
