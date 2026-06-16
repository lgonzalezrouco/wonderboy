-- | FSM reactivo y clases de enemigo (M13): sensado y presets con fixtures fijos.
module Domain.EnemyFsmTest where

import Domain.Fixtures (dtFrame, floorWorld, testParams, worldWithEnemyAt)
import Domain.Logic.Combat (resolveCombat)
import Domain.Logic.EntityBehaviours (defaultProgramForKind, patrolHorizontal)
import Domain.Logic.RunBehaviour (runBehaviourStep)
import Domain.Logic.Step (advanceFrame)
import Domain.Model.Enemy (
  Enemy (..),
  enemyFacing,
  enemyHealth,
  enemyPos,
  enemyVel,
  spawnEnemy,
 )
import Domain.Model.EnemyKind (EnemyKind (..))
import Domain.Model.Player (
  Player (..),
  spawnPlayer,
 )
import Domain.Model.World (World (..), defaultMaxHealth)
import Domain.ValueObjects.CombatParams (CombatParams (..), combatParams)
import Domain.ValueObjects.Facing (Facing (..))
import Domain.ValueObjects.Input (noInput)
import Domain.ValueObjects.Position (posX, position)
import Domain.ValueObjects.Velocity (velX)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))

runBehaviourN :: Int -> World -> World
runBehaviourN 0 w = w
runBehaviourN n w = runBehaviourN (n - 1) (runBehaviourStep w)

enemyFrom :: World -> Enemy
enemyFrom w = case worldEnemies w of
  e : _ -> e
  [] -> error "enemyFrom: no enemies"

testCombatParams :: CombatParams
testCombatParams = combatParams 6 60 1 20.0

unit_snailPatrolMoves :: Assertion
unit_snailPatrolMoves =
  let w0 = worldWithEnemyAt SnailKind (position 40 8) (position (-200) 8)
      wN = iterate (advanceFrame testParams dtFrame noInput) w0 !! 120
   in case worldEnemies wN of
        e : _ -> posX (enemyPos e) /= 40 @?= True
        [] -> assertFailure "expected snail"

unit_batChasesInRange :: Assertion
unit_batChasesInRange =
  let w0 = worldWithEnemyAt BatKind (position 80 8) (position 0 8)
      w1 = runBehaviourN 2 w0
      e = enemyFrom w1
   in velX (enemyVel e) @?= (-80)

unit_batReturnsTowardSpawn :: Assertion
unit_batReturnsTowardSpawn =
  let bat =
        (spawnEnemy 1 BatKind (position 80 8) (defaultProgramForKind BatKind))
          { enemyPos = position 120 8
          }
      w0 =
        floorWorld
          { worldPlayer = spawnPlayer defaultMaxHealth (position (-200) 8)
          , worldEnemies = [bat]
          }
      w1 = runBehaviourN 3 w0
      e = enemyFrom w1
   in velX (enemyVel e) @?= (-40)

unit_golemGuardFacesPlayer :: Assertion
unit_golemGuardFacesPlayer =
  let w0 = worldWithEnemyAt GolemKind (position 170 8) (position 0 8)
      w1 = runBehaviourN 3 w0
      e = enemyFrom w1
   in enemyFacing e @?= FacingLeft

unit_golemChasesOnAlert :: Assertion
unit_golemChasesOnAlert =
  let w0 = worldWithEnemyAt GolemKind (position 100 8) (position 50 8)
      w1 = runBehaviourN 2 w0
      e = enemyFrom w1
   in velX (enemyVel e) @?= (-25)

unit_chaseRangeBoundaryInclusive :: Assertion
unit_chaseRangeBoundaryInclusive =
  let w0 = worldWithEnemyAt BatKind (position 0 8) (position 120 8)
      w1 = runBehaviourN 2 w0
      e = enemyFrom w1
   in velX (enemyVel e) @?= 80

unit_golemSurvivesFirstMelee :: Assertion
unit_golemSurvivesFirstMelee =
  let p =
        (spawnPlayer 3 (position 170 8))
          { playerAttackFrames = 3
          , playerFacing = FacingRight
          }
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies =
              [spawnEnemy 1 GolemKind (position 170 8) (defaultProgramForKind GolemKind)]
          }
      w' = resolveCombat testCombatParams noInput w
   in case worldEnemies w' of
        [e] -> enemyHealth e @?= 1
        _ -> assertFailure "golem should survive one hit"

unit_golemDiesOnSecondMelee :: Assertion
unit_golemDiesOnSecondMelee =
  let p =
        (spawnPlayer 3 (position 170 8))
          { playerAttackFrames = 3
          , playerFacing = FacingRight
          }
      golem =
        (spawnEnemy 1 GolemKind (position 170 8) (defaultProgramForKind GolemKind))
          { enemyHealth = 1
          }
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [golem]
          }
      w' = resolveCombat testCombatParams noInput w
   in worldEnemies w' @?= []

unit_snailDiesInOneMelee :: Assertion
unit_snailDiesInOneMelee =
  let p =
        (spawnPlayer 3 (position 40 8))
          { playerAttackFrames = 3
          , playerFacing = FacingRight
          }
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies =
              [spawnEnemy 1 SnailKind (position 40 8) (patrolHorizontal 30 90)]
          }
      w' = resolveCombat testCombatParams noInput w
   in worldEnemies w' @?= []
