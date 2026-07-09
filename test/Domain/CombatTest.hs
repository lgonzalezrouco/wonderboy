module Domain.CombatTest where

import Domain.Fixtures (floorWorld, swingToImpact, testCombatParams)
import Domain.Logic.BehaviourCatalog (patrolHorizontal)
import Domain.Logic.Combat (resolveCombat)
import Domain.Model.Enemy (enemyHealth, mkEnemy, spawnEnemy)
import Domain.Model.EnemyKind (EnemyKind (..))
import Domain.Model.Player (
  Player (..),
  playerAttackFrames,
  playerHealth,
  playerInvincibilityFrames,
  spawnPlayer,
 )
import Domain.Model.World (World (..))
import Domain.ValueObjects.CombatParams (CombatParams (..))
import Domain.ValueObjects.Facing (Facing (..))
import Domain.ValueObjects.Frames (frameCount, frames, noFrames)
import Domain.ValueObjects.Health (health)
import Domain.ValueObjects.Input (Input (..), noInput)
import Domain.ValueObjects.Position (position)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))

unit_attackEdgeStartsWindow :: Assertion
unit_attackEdgeStartsWindow =
  let w' = resolveCombat testCombatParams (noInput{inputAttack = True}) floorWorld
   in playerAttackFrames (worldPlayer w') @?= cpAttackDuration testCombatParams

unit_meleeRemovesEnemy :: Assertion
unit_meleeRemovesEnemy =
  let p = spawnPlayer (health 3) (position 0 8)
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [mkEnemy 1 (position 40 8) (patrolHorizontal 10 (frames 10))]
          }
      w' = swingToImpact testCombatParams w
   in worldEnemies w' @?= []

unit_meleeRemovesOverlappingEnemyBehind :: Assertion
unit_meleeRemovesOverlappingEnemyBehind =
  let p = spawnPlayer (health 3) (position 50 8)
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [mkEnemy 1 (position 40 8) (patrolHorizontal 10 (frames 10))]
          }
      w' = swingToImpact testCombatParams w
   in worldEnemies w' @?= []

unit_meleeAttackEdgeWhileOverlapping :: Assertion
unit_meleeAttackEdgeWhileOverlapping =
  let p = spawnPlayer (health 3) (position 50 8)
      enemy = mkEnemy 1 (position 50 8) (patrolHorizontal 10 (frames 10))
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [enemy]
          }
      w' = swingToImpact testCombatParams w
   in worldEnemies w' @?= []

unit_meleeInputSwingHitsOnce :: Assertion
unit_meleeInputSwingHitsOnce =
  let golem = spawnEnemy 1 GolemKind (position 40 8) (patrolHorizontal 10 (frames 10))
      attacker = spawnPlayer (health 3) (position 0 8)
      w0 = floorWorld{worldPlayer = attacker, worldEnemies = [golem]}
      wImpact = swingToImpact testCombatParams w0
      wAfter = resolveCombat testCombatParams noInput wImpact
   in case worldEnemies wAfter of
        [e] -> enemyHealth e @?= health 1
        other -> assertFailure ("el Golem debería sobrevivir con 1 HP; quedó: " <> show other)

unit_noMeleeOutsideWindow :: Assertion
unit_noMeleeOutsideWindow =
  let p =
        (spawnPlayer (health 3) (position 0 8))
          { playerAttackFrames = noFrames
          , playerFacing = FacingRight
          }
      enemy = mkEnemy 1 (position 40 8) (patrolHorizontal 10 (frames 10))
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [enemy]
          }
      w' = resolveCombat testCombatParams noInput w
   in worldEnemies w' @?= [enemy]

unit_sideContactDamages :: Assertion
unit_sideContactDamages =
  let p = spawnPlayer (health 3) (position 24 8)
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [mkEnemy 1 (position 40 8) (patrolHorizontal 10 (frames 10))]
          }
      w' = resolveCombat testCombatParams noInput w
   in do
        playerHealth (worldPlayer w') @?= health 2
        playerInvincibilityFrames (worldPlayer w') @?= frames 59

unit_topContactDamages :: Assertion
unit_topContactDamages =
  let p = spawnPlayer (health 3) (position 0 32)
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [mkEnemy 1 (position 0 8) (patrolHorizontal 10 (frames 10))]
          }
      w' = resolveCombat testCombatParams noInput w
   in do
        playerHealth (worldPlayer w') @?= health 2
        playerInvincibilityFrames (worldPlayer w') @?= frames 59

unit_attackDirectionLatched :: Assertion
unit_attackDirectionLatched =
  let p =
        (spawnPlayer (health 3) (position 0 8))
          { playerAttackFrames = cpAttackDuration testCombatParams
          , playerFacing = FacingRight
          }
      enemy = mkEnemy 1 (position (-40) 8) (patrolHorizontal 10 (frames 10))
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [enemy]
          }
      w' = resolveCombat testCombatParams (noInput{inputLeft = True}) w
   in do
        playerFacing (worldPlayer w') @?= FacingRight
        worldEnemies w' @?= [enemy]

unit_iframesBlockRehit :: Assertion
unit_iframesBlockRehit =
  let p =
        (spawnPlayer (health 3) (position 24 8))
          { playerInvincibilityFrames = frames 30
          }
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [mkEnemy 1 (position 40 8) (patrolHorizontal 10 (frames 10))]
          }
      w' = resolveCombat testCombatParams noInput w
   in playerHealth (worldPlayer w') @?= health 3

unit_multiEnemyContactOnce :: Assertion
unit_multiEnemyContactOnce =
  let p = spawnPlayer (health 3) (position 24 8)
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies =
              [ mkEnemy 1 (position 40 8) (patrolHorizontal 10 (frames 10))
              , mkEnemy 2 (position 44 8) (patrolHorizontal 10 (frames 10))
              ]
          }
      w' = resolveCombat testCombatParams noInput w
   in playerHealth (worldPlayer w') @?= health 2

unit_meleeWindowCountsDownAndRearms :: Assertion
unit_meleeWindowCountsDownAndRearms =
  let golem = spawnEnemy 1 GolemKind (position 40 8) (patrolHorizontal 10 (frames 10))
      attacker = spawnPlayer (health 3) (position 0 8)
      w0 = floorWorld{worldPlayer = attacker, worldEnemies = [golem]}
      wStarted = resolveCombat testCombatParams (noInput{inputAttack = True}) w0
      wAfterWindow =
        iterate (resolveCombat testCombatParams noInput) wStarted
          !! frameCount (cpAttackDuration testCombatParams)
      wRearmed = resolveCombat testCombatParams (noInput{inputAttack = True}) wAfterWindow
   in do
        case worldEnemies wAfterWindow of
          [e] -> enemyHealth e @?= health 1
          _ -> assertFailure "golem should take exactly one melee hit per swing"
        playerAttackFrames (worldPlayer wAfterWindow) @?= noFrames
        playerAttackFrames (worldPlayer wRearmed) @?= cpAttackDuration testCombatParams

unit_iframesLastFullDuration :: Assertion
unit_iframesLastFullDuration =
  let p = spawnPlayer (health 3) (position 24 8)
      enemy = mkEnemy 1 (position 40 8) (patrolHorizontal 10 (frames 10))
      w0 =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [enemy]
          }
      wHit = resolveCombat testCombatParams noInput w0
      wImmune = iterate (resolveCombat testCombatParams noInput) wHit !! 59
      wVulnerable = resolveCombat testCombatParams noInput wImmune
   in do
        playerHealth (worldPlayer wHit) @?= health 2
        playerHealth (worldPlayer wImmune) @?= health 2
        playerHealth (worldPlayer wVulnerable) @?= health 1
