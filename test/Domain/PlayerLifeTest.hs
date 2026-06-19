-- | Pure player life, damage, and out-of-bounds tests.
module Domain.PlayerLifeTest where

import Domain.Fixtures (testPlayerProjectile)
import Domain.Logic.PlayerLife (
  applyDamage,
  resolveHazardsAndDeath,
 )
import Domain.Model.ExitZone (defaultExitZone)
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.MovingPlatform (MovingPlatform, mkMovingPlatform)
import Domain.Model.Platform (platform)
import Domain.Model.Player (
  Player (..),
  playerHealth,
  playerInvincibilityFrames,
  playerPos,
  playerVel,
  spawnPlayer,
 )
import Domain.Model.World (World (..))
import Domain.ValueObjects.Damage (damage)
import Domain.ValueObjects.Frames (frames)
import Domain.ValueObjects.Health (health)
import Domain.ValueObjects.LifeParams (LifeParams (..), lifeParams)
import Domain.ValueObjects.Lives (lives)
import Domain.ValueObjects.Position (Position, position)
import Domain.ValueObjects.Score (score)
import Domain.ValueObjects.Velocity (velX, velY, velocity)
import Test.Tasty.HUnit (Assertion, (@?=))

testLifeParams :: LifeParams
testLifeParams = lifeParams (health 3) 64 (frames 60)

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
    }

belowFloor :: Position
belowFloor = position 0 (-100)

deadBelowFloor :: World
deadBelowFloor =
  floorWorld{worldPlayer = (spawnPlayer (health 3) belowFloor){playerHealth = health 0}}

unit_applyDamageReducesHealth :: Assertion
unit_applyDamageReducesHealth =
  playerHealth (applyDamage (damage 1) (spawnPlayer (health 3) testSpawn)) @?= health 2

unit_applyDamageClampsAtZero :: Assertion
unit_applyDamageClampsAtZero =
  playerHealth (applyDamage (damage 10) (spawnPlayer (health 3) testSpawn)) @?= health 0

unit_oobBelowLowestPlatform :: Assertion
unit_oobBelowLowestPlatform =
  let w = floorWorld{worldPlayer = spawnPlayer (health 3) belowFloor}
      (w', lives', phase) = resolveHazardsAndDeath testLifeParams (lives 3) Playing w
   in do
        lives' @?= lives 2
        phase @?= Playing
        playerHealth (worldPlayer w') @?= health 3
        playerPos (worldPlayer w') @?= testSpawn

unit_oobAboveDeathLine :: Assertion
unit_oobAboveDeathLine =
  let w = floorWorld{worldPlayer = spawnPlayer (health 3) (position 0 8)}
      (w', lives', phase) = resolveHazardsAndDeath testLifeParams (lives 3) Playing w
   in do
        playerHealth (worldPlayer w') @?= health 3
        lives' @?= lives 3
        phase @?= Playing

unit_deathWithLivesRemaining :: Assertion
unit_deathWithLivesRemaining =
  let (w', lives', phase) = resolveHazardsAndDeath testLifeParams (lives 3) Playing deadBelowFloor
   in do
        lives' @?= lives 2
        phase @?= Playing
        playerHealth (worldPlayer w') @?= health 3
        playerPos (worldPlayer w') @?= testSpawn

unit_respawnGrantsInvincibility :: Assertion
unit_respawnGrantsInvincibility =
  let (w', lives', phase) = resolveHazardsAndDeath testLifeParams (lives 3) Playing deadBelowFloor
   in do
        lives' @?= lives 2
        phase @?= Playing
        playerInvincibilityFrames (worldPlayer w') @?= frames 60

unit_deathOnLastLife :: Assertion
unit_deathOnLastLife =
  let (w', lives', phase) = resolveHazardsAndDeath testLifeParams (lives 1) Playing deadBelowFloor
   in do
        lives' @?= lives 0
        phase @?= GameOver
        playerHealth (worldPlayer w') @?= health 0

unit_respawnResetsVelocity :: Assertion
unit_respawnResetsVelocity =
  let w =
        deadBelowFloor
          { worldPlayer =
              (worldPlayer deadBelowFloor){playerVel = velocity 99 99}
          }
      (w', _, _) = resolveHazardsAndDeath testLifeParams (lives 3) Playing w
   in do
        velX (playerVel (worldPlayer w')) @?= 0
        velY (playerVel (worldPlayer w')) @?= 0

unit_respawnClearsProjectiles :: Assertion
unit_respawnClearsProjectiles =
  let flying = testPlayerProjectile 1 (position 40 40) (velocity 100 100) (frames 60)
      w =
        deadBelowFloor
          { worldProjectiles = [flying]
          , worldNextProjectileId = 2
          , worldFallingHazards = []
          , worldCrumblingPlatforms = []
          , worldBossArena = Nothing
          }
      (w', _, _) = resolveHazardsAndDeath testLifeParams (lives 3) Playing w
   in worldProjectiles w' @?= []

mustMovingPlatform :: Maybe MovingPlatform -> MovingPlatform
mustMovingPlatform (Just mp) = mp
mustMovingPlatform Nothing = error "lowMovingPlatform: invalid fixture"

-- | Plataforma móvil cuyo borde inferior (y = 100) es el sólido más bajo del mundo.
lowMovingPlatform :: MovingPlatform
lowMovingPlatform =
  mustMovingPlatform
    (mkMovingPlatform 1 (position 0 100) 48 8 (position 0 100) (position 60 100) 35 True)

-- | Mundo sin plataformas estáticas: la línea de muerte la fija la plataforma móvil.
movingPlatformWorld :: World
movingPlatformWorld =
  floorWorld
    { worldPlatforms = []
    , worldMovingPlatforms = [lowMovingPlatform]
    }

{- | Línea de muerte = 100 (borde inferior móvil) − 64 (margen) = 36; por debajo es
  out-of-bounds aunque no haya plataformas estáticas (ejercita el fold sobre móviles).
-}
unit_oobBelowMovingPlatform :: Assertion
unit_oobBelowMovingPlatform =
  let w = movingPlatformWorld{worldPlayer = spawnPlayer (health 3) (position 0 10)}
      (w', lives', phase) = resolveHazardsAndDeath testLifeParams (lives 3) Playing w
   in do
        lives' @?= lives 2
        phase @?= Playing
        playerPos (worldPlayer w') @?= testSpawn

unit_safeAboveMovingPlatformDeathLine :: Assertion
unit_safeAboveMovingPlatformDeathLine =
  let w = movingPlatformWorld{worldPlayer = spawnPlayer (health 3) (position 0 50)}
      (_, lives', phase) = resolveHazardsAndDeath testLifeParams (lives 3) Playing w
   in do
        lives' @?= lives 3
        phase @?= Playing
