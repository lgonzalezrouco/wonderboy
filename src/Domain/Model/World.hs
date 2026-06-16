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

import Domain.Model.Enemy (Enemy)
import Domain.Model.Pickup (Pickup)
import Domain.Model.Platform (Platform, platform)
import Domain.Model.Player (Player (..), spawnPlayer)
import Domain.ValueObjects.Position (Position, position)

-- | Punto de spawn del jugador en este nivel (respawn tras perder una vida).
defaultMaxHealth :: Int
defaultMaxHealth = 3

-- | Estado completo de la simulación.
data World = World
  { worldPlayer :: Player
  , worldEnemies :: [Enemy]
  , worldPlatforms :: [Platform]
  , worldSpawnPoint :: Position
  , worldPickups :: [Pickup]
  , worldMinScore :: Int
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
        , worldSpawnPoint = spawn
        , worldPickups = []
        , worldMinScore = 0
        }
