-- | Pure melee combat and enemy contact tests.
module Domain.CombatTest where

import Domain.Logic.Combat (resolveCombat)
import Domain.Logic.EntityBehaviours (patrolHorizontal)
import Domain.Model.Enemy (mkEnemy)
import Domain.Model.Platform (platform)
import Domain.Model.Player (
  Player (..),
  playerAttackFrames,
  playerHealth,
  playerInvincibilityFrames,
  spawnPlayer,
 )
import Domain.Model.World (World (..))
import Domain.ValueObjects.CombatParams (CombatParams (..), combatParams)
import Domain.ValueObjects.Facing (Facing (..))
import Domain.ValueObjects.Input (Input (..), noInput)
import Domain.ValueObjects.Position (Position, position)
import Test.Tasty.HUnit (Assertion, (@?=))

testCombatParams :: CombatParams
testCombatParams = combatParams 6 60 1 20.0

testSpawn :: Position
testSpawn = position 0 80

floorWorld :: World
floorWorld =
  World
    { worldPlayer = spawnPlayer 3 testSpawn
    , worldEnemies = []
    , worldPlatforms = [platform (position (-200) 0) 400 8]
    , worldSpawnPoint = testSpawn
    , worldPickups = []
    , worldMinScore = 0
    }

unit_attackEdgeStartsWindow :: Assertion
unit_attackEdgeStartsWindow =
  let w' = resolveCombat testCombatParams (noInput{inputAttack = True}) floorWorld
   in playerAttackFrames (worldPlayer w') @?= 6

unit_meleeRemovesEnemy :: Assertion
unit_meleeRemovesEnemy =
  let p =
        (spawnPlayer 3 (position 0 8))
          { playerAttackFrames = 3
          , playerFacing = FacingRight
          }
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [mkEnemy 1 (position 40 8) (patrolHorizontal 10 10)]
          }
      w' = resolveCombat testCombatParams noInput w
   in worldEnemies w' @?= []

unit_meleeRemovesEnemyWhenOverlapping :: Assertion
unit_meleeRemovesEnemyWhenOverlapping =
  let p =
        (spawnPlayer 3 (position 50 8))
          { playerAttackFrames = 3
          , playerFacing = FacingRight
          }
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [mkEnemy 1 (position 50 8) (patrolHorizontal 10 10)]
          }
      w' = resolveCombat testCombatParams noInput w
   in worldEnemies w' @?= []

unit_meleeAttackEdgeWhileOverlapping :: Assertion
unit_meleeAttackEdgeWhileOverlapping =
  let p = spawnPlayer 3 (position 50 8)
      enemy = mkEnemy 1 (position 50 8) (patrolHorizontal 10 10)
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
        (spawnPlayer 3 (position 0 8))
          { playerAttackFrames = 0
          , playerFacing = FacingRight
          }
      enemy = mkEnemy 1 (position 40 8) (patrolHorizontal 10 10)
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [enemy]
          }
      w' = resolveCombat testCombatParams noInput w
   in worldEnemies w' @?= [enemy]

unit_sideContactDamages :: Assertion
unit_sideContactDamages =
  let p = spawnPlayer 3 (position 24 8)
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [mkEnemy 1 (position 40 8) (patrolHorizontal 10 10)]
          }
      w' = resolveCombat testCombatParams noInput w
   in do
        playerHealth (worldPlayer w') @?= 2
        playerInvincibilityFrames (worldPlayer w') @?= 59

-- | Sin pisotón seguro: tocar al enemigo desde arriba también daña.
unit_topContactDamages :: Assertion
unit_topContactDamages =
  let p = spawnPlayer 3 (position 0 32)
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [mkEnemy 1 (position 0 8) (patrolHorizontal 10 10)]
          }
      w' = resolveCombat testCombatParams noInput w
   in do
        playerHealth (worldPlayer w') @?= 2
        playerInvincibilityFrames (worldPlayer w') @?= 59

-- | La dirección del swing queda fija al iniciar: tocar Izquierda a mitad no la cambia.
unit_attackDirectionLatched :: Assertion
unit_attackDirectionLatched =
  let p =
        (spawnPlayer 3 (position 0 8))
          { playerAttackFrames = 3
          , playerFacing = FacingRight
          }
      enemy = mkEnemy 1 (position (-40) 8) (patrolHorizontal 10 10)
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
        (spawnPlayer 3 (position 24 8))
          { playerInvincibilityFrames = 30
          }
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [mkEnemy 1 (position 40 8) (patrolHorizontal 10 10)]
          }
      w' = resolveCombat testCombatParams noInput w
   in playerHealth (worldPlayer w') @?= 3

unit_multiEnemyContactOnce :: Assertion
unit_multiEnemyContactOnce =
  let p = spawnPlayer 3 (position 24 8)
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies =
              [ mkEnemy 1 (position 40 8) (patrolHorizontal 10 10)
              , mkEnemy 2 (position 44 8) (patrolHorizontal 10 10)
              ]
          }
      w' = resolveCombat testCombatParams noInput w
   in playerHealth (worldPlayer w') @?= 2

unit_iframesLastFullDuration :: Assertion
unit_iframesLastFullDuration =
  let p = spawnPlayer 3 (position 24 8)
      enemy = mkEnemy 1 (position 40 8) (patrolHorizontal 10 10)
      w0 =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [enemy]
          }
      wHit = resolveCombat testCombatParams noInput w0
      wImmune = iterate (resolveCombat testCombatParams noInput) wHit !! 59
      wVulnerable = resolveCombat testCombatParams noInput wImmune
   in do
        playerHealth (worldPlayer wHit) @?= 2
        playerHealth (worldPlayer wImmune) @?= 2
        playerHealth (worldPlayer wVulnerable) @?= 1
