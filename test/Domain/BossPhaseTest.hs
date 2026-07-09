module Domain.BossPhaseTest where

import Data.List (find)
import Data.Maybe (fromMaybe)

import Domain.Fixtures (floorWorld, testCombatParams)
import Domain.Logic.BehaviourCatalog (patrolHorizontal)
import Domain.Logic.BossCatalog (bossDefinitionForKind)
import Domain.Logic.BossPhase (resolveBossPhases)
import Domain.Model.BossPhase (
  bossMaxHealth,
  bossPhaseIndex,
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
import Domain.Model.World (World (..))
import Domain.ValueObjects.Frames (frames)
import Domain.ValueObjects.Health (Health, health)
import Domain.ValueObjects.Position (Position, position)
import Test.Tasty.HUnit (Assertion, (@?=))

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

worldWithGolemKing :: World
worldWithGolemKing = floorWorld{worldEnemies = [golemKingAt]}

bossIn :: World -> Enemy
bossIn w =
  fromMaybe (error "bossIn: no boss") $
    find (isBossKind . enemyKind) (worldEnemies w)

phaseIndex :: Enemy -> Int
phaseIndex e = maybe (-1) bossPhaseNumber (enemyBossPhase e)

setBossHealth :: World -> Health -> World
setBossHealth w hp =
  w{worldEnemies = map setHp (worldEnemies w)}
 where
  setHp e
    | isBossKind (enemyKind e) = e{enemyHealth = hp}
    | otherwise = e

transitionFrom :: World -> World -> World
transitionFrom = resolveBossPhases testCombatParams

damageBossTo :: Health -> World -> World
damageBossTo hp w = transitionFrom w (setBossHealth w hp)

unit_bossStartsPhaseZero :: Assertion
unit_bossStartsPhaseZero =
  phaseIndex golemKingAt @?= 0

unit_bossTransitionsAt66Percent :: Assertion
unit_bossTransitionsAt66Percent =
  let w1 = damageBossTo (health 13) worldWithGolemKing
   in phaseIndex (bossIn w1) @?= 1

unit_bossTransitionsAt33Percent :: Assertion
unit_bossTransitionsAt33Percent =
  let w1 = damageBossTo (health 6) worldWithGolemKing
   in phaseIndex (bossIn w1) @?= 2

unit_bossStaysPhaseZeroAboveThreshold :: Assertion
unit_bossStaysPhaseZeroAboveThreshold =
  let w1 = damageBossTo (health 14) worldWithGolemKing
   in phaseIndex (bossIn w1) @?= 0

unit_bossPhasesMonotonic :: Assertion
unit_bossPhasesMonotonic =
  let king = golemKingAt{enemyBossPhase = Just (bossPhaseIndex 2), enemyHealth = health 20}
      w0 = floorWorld{worldEnemies = [king]}
      w1 = transitionFrom w0 w0
   in phaseIndex (bossIn w1) @?= 2

unit_bossSkipsIntermediatePhase :: Assertion
unit_bossSkipsIntermediatePhase =
  let w1 = damageBossTo (health 1) worldWithGolemKing
   in phaseIndex (bossIn w1) @?= 2

unit_nonBossUnchanged :: Assertion
unit_nonBossUnchanged =
  let snail = spawnEnemy 2 SnailKind (position 40 8) (patrolHorizontal 30 (frames 90))
      w0 = floorWorld{worldEnemies = [snail]}
      w1 = transitionFrom w0 w0
   in worldEnemies w1 @?= worldEnemies w0
