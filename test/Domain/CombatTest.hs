-- | Pure melee combat and enemy contact tests.
module Domain.CombatTest where

import Domain.Fixtures (testCombatParams)
import Domain.Logic.Combat (resolveCombat)
import Domain.Logic.EntityBehaviours (patrolHorizontal)
import Domain.Model.Enemy (enemyHealth, mkEnemy, spawnEnemy)
import Domain.Model.EnemyKind (EnemyKind (..))
import Domain.Model.ExitZone (defaultExitZone)
import Domain.Model.Platform (platform)
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
import Domain.ValueObjects.Frames (Frames, frameCount, frames, noFrames)
import Domain.ValueObjects.Health (health)
import Domain.ValueObjects.Input (Input (..), noInput)
import Domain.ValueObjects.Position (Position, position)
import Domain.ValueObjects.Score (score)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))

testSpawn :: Position
testSpawn = position 0 80

floorWorld :: World
floorWorld =
  World
    { worldPlayer = spawnPlayer (health 3) testSpawn
    , worldEnemies = []
    , worldPlatforms = [platform (position (-200) 0) 400 8]
    , worldMovingPlatforms = []
    , worldSpawnPoint = testSpawn
    , worldPickups = []
    , worldMinScore = score 0
    , worldExit = defaultExitZone
    }

unit_attackEdgeStartsWindow :: Assertion
unit_attackEdgeStartsWindow =
  let w' = resolveCombat testCombatParams (noInput{inputAttack = True}) floorWorld
   in playerAttackFrames (worldPlayer w') @?= cpAttackDuration testCombatParams

unit_meleeRemovesEnemy :: Assertion
unit_meleeRemovesEnemy =
  let p =
        (spawnPlayer (health 3) (position 0 8))
          { playerAttackFrames = cpAttackDuration testCombatParams
          , playerFacing = FacingRight
          }
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [mkEnemy 1 (position 40 8) (patrolHorizontal 10 (frames 10))]
          }
      w' = resolveCombat testCombatParams noInput w
   in worldEnemies w' @?= []

unit_meleeRemovesEnemyWhenOverlapping :: Assertion
unit_meleeRemovesEnemyWhenOverlapping =
  let p =
        (spawnPlayer (health 3) (position 50 8))
          { playerAttackFrames = cpAttackDuration testCombatParams
          , playerFacing = FacingRight
          }
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [mkEnemy 1 (position 50 8) (patrolHorizontal 10 (frames 10))]
          }
      w' = resolveCombat testCombatParams noInput w
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
      w' = resolveCombat testCombatParams (noInput{inputAttack = True}) w
   in worldEnemies w' @?= []

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

-- | Sin pisotón seguro: tocar al enemigo desde arriba también daña.
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

-- | La dirección del swing queda fija al iniciar: tocar Izquierda a mitad no la cambia.
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

{- | El melee conecta una sola vez en el frame de inicio del swing: un Golem (salud 2)
queda con salud 1 tras toda la ventana activa, el contador de ataque baja a 0, y un nuevo
press lo rearma a 'cpAttackDuration'.
-}
unit_meleeWindowCountsDownAndRearms :: Assertion
unit_meleeWindowCountsDownAndRearms =
  let golem = spawnEnemy 1 GolemKind (position 40 8) (patrolHorizontal 10 (frames 10))
      attacker =
        (spawnPlayer (health 3) (position 0 8))
          { playerAttackFrames = cpAttackDuration testCombatParams
          , playerFacing = FacingRight
          }
      w0 = floorWorld{worldPlayer = attacker, worldEnemies = [golem]}
      wAfterWindow =
        iterate (resolveCombat testCombatParams noInput) w0
          !! framesToStepCount (cpAttackDuration testCombatParams)
      wRearmed = resolveCombat testCombatParams (noInput{inputAttack = True}) wAfterWindow
   in do
        case worldEnemies wAfterWindow of
          [e] -> enemyHealth e @?= health 1
          _ -> assertFailure "golem should take exactly one melee hit per swing"
        playerAttackFrames (worldPlayer wAfterWindow) @?= noFrames
        playerAttackFrames (worldPlayer wRearmed) @?= cpAttackDuration testCombatParams

framesToStepCount :: Frames -> Int
framesToStepCount = frameCount

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
