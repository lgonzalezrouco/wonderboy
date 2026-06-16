-- | Pure player life, damage, and out-of-bounds tests.
module Domain.PlayerLifeTest where

import Domain.Logic.PlayerLife (
  applyDamage,
  resolveHazardsAndDeath,
 )
import Domain.Model.GamePhase (GamePhase (..))
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
import Domain.ValueObjects.LifeParams (LifeParams (..), lifeParams)
import Domain.ValueObjects.Position (Position, position)
import Domain.ValueObjects.Velocity (velX, velY, velocity)
import Test.Tasty.HUnit (Assertion, (@?=))

testLifeParams :: LifeParams
testLifeParams = lifeParams 3 64 60

testSpawn :: Position
testSpawn = position 0 80

floorWorld :: World
floorWorld =
  World
    { worldPlayer = spawnPlayer 3 testSpawn
    , worldEnemies = []
    , worldPlatforms = [platform (position (-200) 0) 400 8]
    , worldSpawnPoint = testSpawn
    }

belowFloor :: Position
belowFloor = position 0 (-100)

deadBelowFloor :: World
deadBelowFloor =
  floorWorld{worldPlayer = (spawnPlayer 3 belowFloor){playerHealth = 0}}

unit_applyDamageReducesHealth :: Assertion
unit_applyDamageReducesHealth =
  playerHealth (applyDamage 1 (spawnPlayer 3 testSpawn)) @?= 2

unit_applyDamageClampsAtZero :: Assertion
unit_applyDamageClampsAtZero =
  playerHealth (applyDamage 10 (spawnPlayer 3 testSpawn)) @?= 0

unit_oobBelowLowestPlatform :: Assertion
unit_oobBelowLowestPlatform =
  let w = floorWorld{worldPlayer = spawnPlayer 3 belowFloor}
      (w', lives, phase) = resolveHazardsAndDeath testLifeParams 3 Playing w
   in do
        lives @?= 2
        phase @?= Playing
        playerHealth (worldPlayer w') @?= 3
        playerPos (worldPlayer w') @?= testSpawn

unit_oobAboveDeathLine :: Assertion
unit_oobAboveDeathLine =
  let w = floorWorld{worldPlayer = spawnPlayer 3 (position 0 8)}
      (w', lives, phase) = resolveHazardsAndDeath testLifeParams 3 Playing w
   in do
        playerHealth (worldPlayer w') @?= 3
        lives @?= 3
        phase @?= Playing

unit_deathWithLivesRemaining :: Assertion
unit_deathWithLivesRemaining =
  let (w', lives, phase) = resolveHazardsAndDeath testLifeParams 3 Playing deadBelowFloor
   in do
        lives @?= 2
        phase @?= Playing
        playerHealth (worldPlayer w') @?= 3
        playerPos (worldPlayer w') @?= testSpawn

unit_respawnGrantsInvincibility :: Assertion
unit_respawnGrantsInvincibility =
  let (w', lives, phase) = resolveHazardsAndDeath testLifeParams 3 Playing deadBelowFloor
   in do
        lives @?= 2
        phase @?= Playing
        playerInvincibilityFrames (worldPlayer w') @?= 60

unit_deathOnLastLife :: Assertion
unit_deathOnLastLife =
  let (w', lives, phase) = resolveHazardsAndDeath testLifeParams 1 Playing deadBelowFloor
   in do
        lives @?= 0
        phase @?= GameOver
        playerHealth (worldPlayer w') @?= 0

unit_respawnResetsVelocity :: Assertion
unit_respawnResetsVelocity =
  let w =
        deadBelowFloor
          { worldPlayer =
              (worldPlayer deadBelowFloor){playerVel = velocity 99 99}
          }
      (w', _, _) = resolveHazardsAndDeath testLifeParams 3 Playing w
   in do
        velX (playerVel (worldPlayer w')) @?= 0
        velY (playerVel (worldPlayer w')) @?= 0
