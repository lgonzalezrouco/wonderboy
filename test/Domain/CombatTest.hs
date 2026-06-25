-- | Pure melee combat and enemy contact tests.
module Domain.CombatTest where

import Domain.Fixtures (testCombatParams)
import Domain.Logic.BehaviourCatalog (patrolHorizontal)
import Domain.Logic.Combat (resolveCombat)
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
    , worldProjectiles = []
    , worldNextProjectileId = 1
    , worldFallingHazards = []
    , worldCrumblingPlatforms = []
    , worldBossArena = Nothing
    , worldBossArenaEngaged = False
    }

unit_attackEdgeStartsWindow :: Assertion
unit_attackEdgeStartsWindow =
  let w' = resolveCombat testCombatParams (noInput{inputAttack = True}) floorWorld
   in playerAttackFrames (worldPlayer w') @?= cpAttackDuration testCombatParams

unit_meleeRemovesEnemy :: Assertion
unit_meleeRemovesEnemy =
  let p = spawnPlayer (health 3) (position 0 8) -- mira a la derecha por defecto
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [mkEnemy 1 (position 40 8) (patrolHorizontal 10 (frames 10))]
          }
      w' = resolveCombat testCombatParams (noInput{inputAttack = True}) w
   in worldEnemies w' @?= []

{- | El melee también conecta con un enemigo que solapa el __cuerpo__ del jugador
aunque quede del lado opuesto al facing: cubre la rama @aabbOverlaps body@ de
'hitsEnemy', distinta del alcance del swing hacia adelante.
-}
unit_meleeRemovesOverlappingEnemyBehind :: Assertion
unit_meleeRemovesOverlappingEnemyBehind =
  -- Jugador mirando a la derecha; enemigo a la izquierda solapando el cuerpo (no el alcance).
  let p = spawnPlayer (health 3) (position 50 8)
      w =
        floorWorld
          { worldPlayer = p
          , worldEnemies = [mkEnemy 1 (position 40 8) (patrolHorizontal 10 (frames 10))]
          }
      w' = resolveCombat testCombatParams (noInput{inputAttack = True}) w
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

{- | Regresión: un swing iniciado por __input real__ conecta UNA sola vez.

El daño debe atarse al frame en que arranca el ataque, no al estado
@playerAttackFrames == cpAttackDuration@: ese estado persiste dos frames cuando el
swing se inicia por input, porque 'decrementAttack' no baja el contador en el frame de
arranque. Antes esto causaba dos golpes por swing y un Golem (salud 2) moría de un solo
press. Acá el Golem debe quedar con salud 1 tras presionar atacar y soltar.
-}
unit_meleeInputSwingHitsOnce :: Assertion
unit_meleeInputSwingHitsOnce =
  let golem = spawnEnemy 1 GolemKind (position 40 8) (patrolHorizontal 10 (frames 10))
      attacker = spawnPlayer (health 3) (position 0 8)
      w0 = floorWorld{worldPlayer = attacker, worldEnemies = [golem]}
      -- Frame de arranque: el jugador presiona atacar.
      w1 = resolveCombat testCombatParams (noInput{inputAttack = True}) w0
      -- Frame siguiente: suelta el botón; no debe volver a pegar.
      w2 = resolveCombat testCombatParams noInput w1
   in case worldEnemies w2 of
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

{- | El melee conecta una sola vez en el frame de inicio del swing: arrancado por input,
un Golem (salud 2) queda con salud 1 tras toda la ventana activa (sin volver a pegar
mientras el contador baja), el contador de ataque llega a 0, y un nuevo press lo rearma a
'cpAttackDuration'.
-}
unit_meleeWindowCountsDownAndRearms :: Assertion
unit_meleeWindowCountsDownAndRearms =
  let golem = spawnEnemy 1 GolemKind (position 40 8) (patrolHorizontal 10 (frames 10))
      attacker = spawnPlayer (health 3) (position 0 8) -- mira a la derecha por defecto
      w0 = floorWorld{worldPlayer = attacker, worldEnemies = [golem]}
      -- Arranca el swing por input; luego la ventana corre sin más presses.
      wStarted = resolveCombat testCombatParams (noInput{inputAttack = True}) w0
      wAfterWindow =
        iterate (resolveCombat testCombatParams noInput) wStarted
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
