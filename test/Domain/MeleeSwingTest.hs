module Domain.MeleeSwingTest where

import Domain.Fixtures (floorWorld, swingToImpact, testCombatParams)
import Domain.Logic.BehaviourCatalog (patrolHorizontal)
import Domain.Logic.Combat (resolveCombat)
import Domain.Logic.MeleeSwing (
  isMeleeImpactFrame,
  meleeHitboxAtImpact,
  meleeImpactFrameCount,
  meleeImpactPhase,
 )
import Domain.Model.Enemy (enemyHealth, enemyHurtFrames, mkEnemy, spawnEnemy)
import Domain.Model.EnemyKind (EnemyKind (..))
import Domain.Model.Player (
  Player (..),
  playerAttackFrames,
  spawnPlayer,
 )
import Domain.Model.World (World (..))
import Domain.ValueObjects.Aabb (Aabb (..), aabbMaxX)
import Domain.ValueObjects.CombatParams (CombatParams (..))
import Domain.ValueObjects.Facing (Facing (..))
import Domain.ValueObjects.Frames (frameCount, frames, noFrames)
import Domain.ValueObjects.Health (health)
import Domain.ValueObjects.Input (Input (..), noInput)
import Domain.ValueObjects.Position (position)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))

unit_impactFrameCountDefault :: Assertion
unit_impactFrameCountDefault =
  meleeImpactFrameCount testCombatParams @?= 5

unit_noDamageOnAttackStart :: Assertion
unit_noDamageOnAttackStart =
  let golem = spawnEnemy 1 GolemKind (position 40 8) (patrolHorizontal 10 (frames 10))
      w0 = floorWorld{worldPlayer = spawnPlayer (health 3) (position 0 8), worldEnemies = [golem]}
      w1 = resolveCombat testCombatParams (noInput{inputAttack = True}) w0
   in case worldEnemies w1 of
        [e] -> enemyHealth e @?= health 2
        other -> assertFailure ("golem no debería dañarse al arrancar el swing: " <> show other)

unit_impactHitboxExtendsBeyondFixedReach :: Assertion
unit_impactHitboxExtendsBeyondFixedReach =
  let body = Aabb{aabbMinX = 0, aabbMinY = 0, aabbMaxX = 32, aabbMaxY = 48}
      fixedReach = body{aabbMaxX = aabbMaxX body + cpMeleeReach testCombatParams}
      impact = meleeHitboxAtImpact body FacingRight
   in aabbMaxX impact > aabbMaxX fixedReach @?= True

unit_golemHurtFlashOnSurvivingMelee :: Assertion
unit_golemHurtFlashOnSurvivingMelee =
  let golem = spawnEnemy 1 GolemKind (position 40 8) (patrolHorizontal 10 (frames 10))
      w0 = floorWorld{worldPlayer = spawnPlayer (health 3) (position 0 8), worldEnemies = [golem]}
      w' = swingToImpact testCombatParams w0
   in case worldEnemies w' of
        [e] -> do
          enemyHealth e @?= health 1
          frameCount (enemyHurtFrames e) @?= frameCount (cpEnemyHurtFlashDuration testCombatParams)
        other -> assertFailure ("golem debería sobrevivir con destello: " <> show other)

unit_snailNoHurtFlashWhenRemoved :: Assertion
unit_snailNoHurtFlashWhenRemoved =
  let w0 =
        floorWorld
          { worldPlayer = spawnPlayer (health 3) (position 0 8)
          , worldEnemies = [mkEnemy 1 (position 40 8) (patrolHorizontal 10 (frames 10))]
          }
      w' = swingToImpact testCombatParams w0
   in worldEnemies w' @?= []

unit_isMeleeImpactFrameOnlyOnImpactCount :: Assertion
unit_isMeleeImpactFrameOnlyOnImpactCount =
  let p =
        (spawnPlayer (health 3) (position 0 8))
          { playerAttackFrames = frames (meleeImpactFrameCount testCombatParams)
          }
   in isMeleeImpactFrame testCombatParams p @?= True

unit_isMeleeImpactFrameFalseOnStart :: Assertion
unit_isMeleeImpactFrameFalseOnStart =
  let p =
        (spawnPlayer (health 3) (position 0 8))
          { playerAttackFrames = cpAttackDuration testCombatParams
          }
   in isMeleeImpactFrame testCombatParams p @?= False

unit_hurtFramesTickDown :: Assertion
unit_hurtFramesTickDown =
  let golem = spawnEnemy 1 GolemKind (position 40 8) (patrolHorizontal 10 (frames 10))
      w0 = floorWorld{worldPlayer = spawnPlayer (health 3) (position 0 8), worldEnemies = [golem]}
      wHit = swingToImpact testCombatParams w0
      wLater =
        iterate (resolveCombat testCombatParams noInput) wHit
          !! frameCount (cpEnemyHurtFlashDuration testCombatParams)
   in case worldEnemies wLater of
        [e] -> enemyHurtFrames e @?= noFrames
        _ -> assertFailure "golem expected after hurt flash expires"

unit_meleeImpactPhaseInRange :: Assertion
unit_meleeImpactPhaseInRange =
  meleeImpactPhase > 0 && meleeImpactPhase < 1 @?= True
