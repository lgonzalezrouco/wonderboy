{- | Estado completo del mundo del juego en un instante dado.

'World' es el tipo que 'GameState' representará en @UseCases.GameMonad@.
Es la "fotografía" de la simulación: todo lo que el motor necesita para
calcular el siguiente frame.

La integración cinemática y colisiones viven en @Domain.Logic.Step@ (M3).
-}
module Domain.Model.World (
  -- * Tipo
  World (..),

  -- * Construcción
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

{- | Salud máxima por defecto del jugador.

Fuente única del valor de salud inicial: 'initialWorld', los fixtures y la
configuración por defecto ('UseCases.GameMonad.defaultConfig' vía @gcMaxHealth@)
la referencian en lugar de repetir el literal.
-}
defaultMaxHealth :: Health
defaultMaxHealth = health 3

-- | Estado completo de la simulación.
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
  -- ^ Jugador comprometido con la arena: las paredes siguen hasta derrotar al jefe.
  }
  deriving (Eq, Show, Generic)

-- | Plataforma de suelo compartida por mundos de prueba y demo.
testFloor :: Platform
testFloor = platform (position (-200) 0) 400 8

{- | Mundo inicial para tests de física del jugador: jugador sobre el suelo, sin enemigos.

El jugador spawnea en @y = 80@ para que la gravedad lo baje hasta el suelo
en tests y demos.
-}
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
