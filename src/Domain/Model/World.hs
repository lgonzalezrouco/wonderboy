module Domain.Model.World (
  World (..),
  initialWorld,
  defaultMaxHealth,
)
where

import GHC.Generics (Generic)

import Domain.Model.BossArena (BossArena)
import Domain.Model.CrumblingPlatform (CrumblingPlatform)
import Domain.Model.Enemy (Enemy)
import Domain.Model.ExitZone (ExitZone, defaultExitZone)
import Domain.Model.FallingHazard (FallingHazard)
import Domain.Model.MovingPlatform (MovingPlatform)
import Domain.Model.Pickup (Pickup)
import Domain.Model.Platform (Platform, platform)
import Domain.Model.Player (Player (..), spawnPlayer)
import Domain.Model.Projectile (Projectile)
import Domain.ValueObjects.Health (Health, health)
import Domain.ValueObjects.Position (Position, position)
import Domain.ValueObjects.Score (Score, score)

defaultMaxHealth :: Health
defaultMaxHealth = health 3

data World = World
  { worldPlayer :: Player
  , worldEnemies :: [Enemy]
  , worldPlatforms :: [Platform]
  , worldMovingPlatforms :: [MovingPlatform]
  , worldSpawnPoint :: Position
  , worldPickups :: [Pickup]
  , worldMinScore :: Score
  , worldExit :: ExitZone
  , worldProjectiles :: [Projectile]
  , worldNextProjectileId :: Int
  , worldFallingHazards :: [FallingHazard]
  , worldCrumblingPlatforms :: [CrumblingPlatform]
  , worldBossArena :: Maybe BossArena
  , worldBossArenaEngaged :: Bool
  -- ^ Una vez que el jugador entra a la arena esto queda en true, manteniendo las paredes hasta que el boss muere.
  }
  deriving (Eq, Show, Generic)

testFloor :: Platform
testFloor = platform (position (-200) 0) 400 8

initialWorld :: World
initialWorld =
  let spawn = position 0 80
   in World
        { worldPlayer = spawnPlayer defaultMaxHealth spawn
        , worldEnemies = []
        , worldPlatforms = [testFloor]
        , worldMovingPlatforms = []
        , worldSpawnPoint = spawn
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
