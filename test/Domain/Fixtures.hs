-- | Shared worlds and stepping helpers for domain smoke tests.
module Domain.Fixtures (
  ceilingPlatform,
  dtFrame,
  fallUntilGround,
  floorWorld,
  mkTestPickup,
  testParams,
  wallPlatform,
  worldGrounded,
  worldWithCeiling,
  worldWithPickups,
  worldWithWall,
  worldWithEnemyAt,
)
where

import Data.Maybe (fromMaybe)

import Domain.Logic.EntityBehaviours (defaultProgramForKind)
import Domain.Logic.Step (step)
import Domain.Model.Enemy (spawnEnemy)
import Domain.Model.EnemyKind (EnemyKind)
import Domain.Model.ExitZone (defaultExitZone)
import Domain.Model.Pickup (Pickup, mkPickup)
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

-- | Spawn point shared by floor-based pickup and combat fixtures.
testSpawn :: Position
testSpawn = position 0 80

-- | Floor world with no enemies or pickups (player at 'testSpawn').
floorWorld :: World
floorWorld =
  World
    { worldPlayer = spawnPlayer defaultMaxHealth testSpawn
    , worldEnemies = []
    , worldPlatforms = [floorPlatform]
    , worldMovingPlatforms = []
    , worldSpawnPoint = testSpawn
    , worldPickups = []
    , worldMinScore = 0
    , worldExit = defaultExitZone
    }

-- | Valid pickup for tests; panics only on negative @value@ (use 'mkPickup' for that case).
mkTestPickup :: Int -> Position -> Int -> Pickup
mkTestPickup pid pos value =
  fromMaybe (error "mkTestPickup: negative pickup value") (mkPickup pid pos value)

-- | 'floorWorld' with the player at @pos@ and the given pickups.
worldWithPickups :: Position -> [Pickup] -> World
worldWithPickups pos pickups =
  floorWorld
    { worldPlayer = spawnPlayer defaultMaxHealth pos
    , worldPickups = pickups
    }

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
    , worldMovingPlatforms = []
    , worldSpawnPoint = position 0 25
    , worldPickups = []
    , worldMinScore = 0
    , worldExit = defaultExitZone
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
    , worldMovingPlatforms = []
    , worldSpawnPoint = position 33 8
    , worldPickups = []
    , worldMinScore = 0
    , worldExit = defaultExitZone
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

-- | Floor world with one enemy of the given kind and a fixed player position.
worldWithEnemyAt :: EnemyKind -> Position -> Position -> World
worldWithEnemyAt kind enemyPos playerPos =
  floorWorld
    { worldPlayer = spawnPlayer defaultMaxHealth playerPos
    , worldEnemies = [spawnEnemy 1 kind enemyPos (defaultProgramForKind kind)]
    }
