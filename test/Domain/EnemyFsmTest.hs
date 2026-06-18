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
import Domain.ValueObjects.Damage (damage)
import Domain.ValueObjects.Facing (Facing (..))
import Domain.ValueObjects.Frames (frames)
import Domain.ValueObjects.Health (health)
import Domain.ValueObjects.Input (noInput)
import Domain.ValueObjects.Position (Position, posX, position)
import Domain.ValueObjects.Velocity (velX, velY)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))

runBehaviourN :: Int -> World -> World
runBehaviourN 0 w = w
runBehaviourN n w = runBehaviourN (n - 1) (runBehaviourStep w)

enemyFrom :: World -> Enemy
enemyFrom w = case worldEnemies w of
  e : _ -> e
  [] -> error "enemyFrom: no enemies"

attackingPlayerAt :: Position -> Player
attackingPlayerAt pos =
  (spawnPlayer (health 3) pos){playerAttackFrames = frames 6, playerFacing = FacingRight}

golemAt :: Position -> Enemy
golemAt pos = spawnEnemy 1 GolemKind pos (defaultProgramForKind GolemKind)

meleeWorld :: Player -> [Enemy] -> World
meleeWorld p enemies = floorWorld{worldPlayer = p, worldEnemies = enemies}

testCombatParams :: CombatParams
testCombatParams = combatParams (frames 6) (frames 60) (damage 1) 20.0 (damage 1)

unit_snailPatrolMoves :: Assertion
unit_snailPatrolMoves =
  let w0 = worldWithEnemyAt SnailKind (position 40 8) (position (-200) 8)
      wN = iterate (advanceFrame testParams dtFrame noInput) w0 !! 120
   in case worldEnemies wN of
        e : _ -> posX (enemyPos e) /= 40 @?= True
        [] -> assertFailure "expected snail"

unit_batChasesInRange :: Assertion
unit_batChasesInRange =
  let w0 = worldWithEnemyAt BatKind (position 80 56) (position 0 8)
      w1 = runBehaviourN 2 w0
      e = enemyFrom w1
   in do
        velX (enemyVel e) @?= (-80)
        velY (enemyVel e) @?= 0

unit_batPatrolsHorizontallyAtSpawn :: Assertion
unit_batPatrolsHorizontallyAtSpawn =
  let w0 = worldWithEnemyAt BatKind (position 80 56) (position (-200) 8)
      w1 = runBehaviourN 4 w0
      e = enemyFrom w1
   in do
        velY (enemyVel e) @?= 0
        abs (velX (enemyVel e)) @?= 40

unit_batReturnsTowardSpawn :: Assertion
unit_batReturnsTowardSpawn =
  let bat =
        (spawnEnemy 1 BatKind (position 80 56) (defaultProgramForKind BatKind))
          { enemyPos = position 120 56
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

unit_golemGuardIdleVelocity :: Assertion
unit_golemGuardIdleVelocity =
  let w0 = worldWithEnemyAt GolemKind (position 170 8) (position 0 8)
      w1 = runBehaviourN 3 w0
      e = enemyFrom w1
   in velX (enemyVel e) @?= 0

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
  let pos = position 170 8
      w = meleeWorld (attackingPlayerAt pos) [golemAt pos]
      w' = resolveCombat testCombatParams noInput w
   in case worldEnemies w' of
        [e] -> enemyHealth e @?= health 1
        _ -> assertFailure "golem should survive one hit"

unit_golemDiesOnSecondMelee :: Assertion
unit_golemDiesOnSecondMelee =
  let pos = position 170 8
      w = meleeWorld (attackingPlayerAt pos) [(golemAt pos){enemyHealth = health 1}]
      w' = resolveCombat testCombatParams noInput w
   in worldEnemies w' @?= []

unit_meleeOneHitPerSwing :: Assertion
unit_meleeOneHitPerSwing =
  let pos = position 170 8
      w0 = meleeWorld (attackingPlayerAt pos) [golemAt pos]
      w1 = resolveCombat testCombatParams noInput w0
      w2 = resolveCombat testCombatParams noInput w1
   in case worldEnemies w2 of
        [e] -> enemyHealth e @?= health 1
        _ -> assertFailure "golem should take only one hit per swing"

unit_snailDiesInOneMelee :: Assertion
unit_snailDiesInOneMelee =
  let pos = position 40 8
      w =
        meleeWorld
          (attackingPlayerAt pos)
          [spawnEnemy 1 SnailKind pos (patrolHorizontal 30 (frames 90))]
      w' = resolveCombat testCombatParams noInput w
   in worldEnemies w' @?= []
