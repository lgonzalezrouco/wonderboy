{-# LANGUAGE OverloadedStrings #-}

-- | Shared worlds and stepping helpers for domain smoke tests.
module Domain.Fixtures (
  ceilingPlatform,
  decodeDemoLevel,
  demoJsonFixture,
  demoWorld,
  dtFrame,
  fallUntilGround,
  floorWorld,
  mkTestPickup,
  testParams,
  testCombatParams,
  testThrowParams,
  testPlayerProjectile,
  testEnemyProjectile,
  wallPlatform,
  worldGrounded,
  worldWithCeiling,
  worldWithPickups,
  worldWithWall,
  enemyFrom,
  runBehaviourN,
  worldWithEnemyAt,
)
where

import Data.Maybe (fromMaybe)

import Data.Aeson (eitherDecodeStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Domain.Logic.BuildWorld (buildWorld)
import Domain.Logic.EntityBehaviours (defaultProgramForKind)
import Domain.Logic.RunBehaviour (runBehaviourStep)
import Domain.Logic.Step (step)
import Domain.Model.Enemy (Enemy (..), spawnEnemy)
import Domain.Model.EnemyKind (EnemyKind)
import Domain.Model.ExitZone (defaultExitZone)
import Domain.Model.LevelDefinition (LevelBuildError (..), LevelDefinition)
import Domain.Model.Pickup (Pickup, mkPickup)
import Domain.Model.Platform (Platform, platform)
import Domain.Model.Player (
  Player (..),
  playerOnGround,
  playerVel,
  spawnPlayer,
 )
import Domain.Model.Projectile (
  Projectile (..),
  ProjectileMotion (Ballistic, Linear),
  ProjectileOwner (EnemyProjectile, PlayerProjectile),
 )
import Domain.Model.World (World (..), defaultMaxHealth, initialWorld)
import Domain.ValueObjects.CombatParams (CombatParams)
import Domain.ValueObjects.DeltaTime (DeltaTime, deltaTime)
import Domain.ValueObjects.Frames (Frames)
import Domain.ValueObjects.Input (noInput)
import Domain.ValueObjects.PhysicsParams (PhysicsParams)
import Domain.ValueObjects.Position (Position, position)
import Domain.ValueObjects.Score (score)
import Domain.ValueObjects.ThrowParams (ThrowParams (..))
import Domain.ValueObjects.Velocity (Velocity, velocity)
import Test.Tasty.HUnit (assertBool)
import UseCases.GameMonad (
  combatParamsFromConfig,
  defaultConfig,
  physicsParamsFromConfig,
  throwParamsFromConfig,
 )

-- | Física y combate alineados con 'defaultConfig' (única fuente de verdad).
testParams :: PhysicsParams
testParams = physicsParamsFromConfig defaultConfig

testCombatParams :: CombatParams
testCombatParams = combatParamsFromConfig defaultConfig

testThrowParams :: ThrowParams
testThrowParams = throwParamsFromConfig defaultConfig

-- | Player-owned ballistic projectile sized from 'testThrowParams'.
testPlayerProjectile :: Int -> Position -> Velocity -> Frames -> Projectile
testPlayerProjectile pid pos vel lifetime =
  Projectile
    { projectileId = pid
    , projectilePos = pos
    , projectileVel = vel
    , projectileLifetime = lifetime
    , projectileMotion = Ballistic
    , projectileOwner = PlayerProjectile
    , projectileWidth = tpWidth testThrowParams
    , projectileHeight = tpHeight testThrowParams
    }

-- | Enemy-owned linear projectile for combat tests.
testEnemyProjectile :: Int -> Position -> Velocity -> Frames -> Projectile
testEnemyProjectile pid pos vel lifetime =
  Projectile
    { projectileId = pid
    , projectilePos = pos
    , projectileVel = vel
    , projectileLifetime = lifetime
    , projectileMotion = Linear
    , projectileOwner = EnemyProjectile
    , projectileWidth = 8
    , projectileHeight = 8
    }

-- | One frame at 60 Hz.
dtFrame :: DeltaTime
dtFrame = deltaTime 0.016

-- | Demo level JSON shared by the load tests and the orchestration fixtures.
demoJsonFixture :: Text
demoJsonFixture =
  "{\"minScore\":150,\"spawn\":{\"x\":-100,\"y\":80},\"platforms\":[{\"pos\":{\"x\":-200,\"y\":0},\"width\":400,\"height\":8},{\"pos\":{\"x\":130,\"y\":24},\"width\":32,\"height\":8},{\"pos\":{\"x\":200,\"y\":48},\"width\":64,\"height\":8}],\"movingPlatforms\":[{\"id\":1,\"pos\":{\"x\":30,\"y\":72},\"width\":48,\"height\":8,\"endA\":{\"x\":30,\"y\":72},\"endB\":{\"x\":90,\"y\":72},\"speed\":35,\"startTowardB\":true}],\"enemies\":[{\"id\":1,\"kind\":\"snail\",\"pos\":{\"x\":40,\"y\":8},\"behaviourHint\":\"patrol back and forth along the ground\"},{\"id\":2,\"kind\":\"bat\",\"pos\":{\"x\":80,\"y\":56},\"behaviourHint\":\"chase the player when they get close, then return to spawn\"},{\"id\":3,\"kind\":\"bossGolem\",\"pos\":{\"x\":220,\"y\":56}}],\"pickups\":[{\"id\":1,\"pos\":{\"x\":-120,\"y\":8},\"value\":100},{\"id\":2,\"pos\":{\"x\":10,\"y\":8},\"value\":50},{\"id\":3,\"pos\":{\"x\":60,\"y\":80},\"value\":200},{\"id\":4,\"pos\":{\"x\":232,\"y\":56},\"value\":75}],\"exit\":{\"pos\":{\"x\":280,\"y\":0},\"width\":32,\"height\":64}}"

-- | Decodes the demo level definition from 'demoJsonFixture'.
decodeDemoLevel :: Either String LevelDefinition
decodeDemoLevel = eitherDecodeStrict (encodeUtf8 demoJsonFixture)

{- | World built from the demo level (shared orchestration fixture).

Panics loudly if the embedded JSON fails to decode or build: it is a known-good
fixture, not user input.
-}
demoWorld :: World
demoWorld =
  case buildWorld <$> decodeDemoLevel of
    Right (Right w) -> w
    Right (Left (LevelBuildError msg)) -> error (show msg)
    Left err -> error err

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
    , worldMinScore = score 0
    , worldExit = defaultExitZone
    , worldProjectiles = []
    , worldNextProjectileId = 1
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
    , worldMinScore = score 0
    , worldExit = defaultExitZone
    , worldProjectiles = []
    , worldNextProjectileId = 1
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
    , worldMinScore = score 0
    , worldExit = defaultExitZone
    , worldProjectiles = []
    , worldNextProjectileId = 1
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

-- | Runs @n@ behaviour steps on the world.
runBehaviourN :: Int -> World -> World
runBehaviourN n w = iterate runBehaviourStep w !! n

-- | First enemy in the world (test fixture; errors if empty).
enemyFrom :: World -> Enemy
enemyFrom w = case worldEnemies w of
  e : _ -> e
  [] -> error "enemyFrom: no enemies"

-- | Floor world with one enemy of the given kind and a fixed player position.
worldWithEnemyAt :: EnemyKind -> Position -> Position -> World
worldWithEnemyAt kind enemyPos playerPos =
  floorWorld
    { worldPlayer = spawnPlayer defaultMaxHealth playerPos
    , worldEnemies = [spawnEnemy 1 kind enemyPos (defaultProgramForKind kind)]
    }
