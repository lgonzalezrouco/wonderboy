-- | Shared worlds and stepping helpers for domain smoke tests.
module Domain.Fixtures (
  ceilingPlatform,
  dtFrame,
  fallUntilGround,
  testParams,
  wallPlatform,
  worldGrounded,
  worldWithCeiling,
  worldWithWall,
)
where

import Domain.Logic.Step (step)
import Domain.Model.Platform (Platform, platform)
import Domain.Model.Player (
  Player (..),
  playerOnGround,
  playerVel,
  spawnPlayer,
 )
import Domain.Model.World (World (..), defaultMaxHealth, initialWorld)
import Domain.ValueObjects.DeltaTime (DeltaTime, deltaTime)
import Domain.ValueObjects.Input (noInput)
import Domain.ValueObjects.PhysicsParams (PhysicsParams (..), physicsParams)
import Domain.ValueObjects.Position (Position, position)
import Domain.ValueObjects.Velocity (velocity)
import Test.Tasty.HUnit (assertBool)

-- | Standard physics constants for tests (px/s, px/s²).
testParams :: PhysicsParams
testParams = physicsParams 980 200 400

-- | One frame at 60 Hz.
dtFrame :: DeltaTime
dtFrame = deltaTime 0.016

-- | Steps until the player is on ground or fails when @n@ reaches zero.
fallUntilGround :: Int -> World -> IO World
fallUntilGround _ w
  | playerOnGround (worldPlayer w) = pure w
fallUntilGround 0 w =
  assertBool "player did not land within step budget" False >> pure w
fallUntilGround n w =
  fallUntilGround (n - 1) (step testParams dtFrame noInput w)

-- | 'initialWorld' after the player has landed on the test floor.
worldGrounded :: IO World
worldGrounded = fallUntilGround 500 initialWorld

-- | Player rising toward a low ceiling (one platform overhead).
worldWithCeiling :: World
worldWithCeiling =
  World
    { worldPlayer = risingPlayer (position 0 25)
    , worldEnemies = []
    , worldPlatforms = [ceilingPlatform]
    , worldSpawnPoint = position 0 25
    }

-- | Player just left of a vertical wall, on a floor strip.
worldWithWall :: World
worldWithWall =
  World
    { worldPlayer = spawnPlayer defaultMaxHealth (position 33 8)
    , worldEnemies = []
    , worldPlatforms =
        [ floorPlatform
        , wallPlatform
        ]
    , worldSpawnPoint = position 33 8
    }

-- | Wall left face at x = 50 (platform bottom-left anchor).
wallPlatform :: Platform
wallPlatform = platform (position 50 0) 8 200

floorPlatform :: Platform
floorPlatform = platform (position (-100) 0) 300 8

-- | Ceiling underside at y = 80.
ceilingPlatform :: Platform
ceilingPlatform = platform (position (-100) 80) 200 8

risingPlayer :: Position -> Player
risingPlayer pos =
  (spawnPlayer defaultMaxHealth pos){playerVel = velocity 0 500, playerOnGround = False}
