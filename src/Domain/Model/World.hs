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
  defaultInitialLives,
  initialWorld,
  mkTestWorld,
)
where

import GHC.Generics (Generic)

import Domain.Model.Enemy (Enemy)
import Domain.Model.GamePhase (GamePhase (Playing))
import Domain.Model.Platform (Platform, platform)
import Domain.Model.Player (Player, spawnPlayer)
import Domain.ValueObjects.Position (Position, position)

-- | Estado completo de la simulación.
data World = World
  { worldPlayer :: Player
  , worldEnemies :: [Enemy]
  , worldPlatforms :: [Platform]
  , worldSpawnPoint :: Position
  -- ^ Punto de pies del jugador para carga inicial y respawn (M9).
  , worldLives :: Int
  -- ^ Stock de vidas restantes (≥ 0).
  , worldPhase :: GamePhase
  -- ^ Fase gruesa: simulación activa o game over.
  }
  deriving (Eq, Show, Generic)

-- | Vidas iniciales por defecto en demos y tests (M9).
defaultInitialLives :: Int
defaultInitialLives = 3

-- | Plataforma de suelo compartida por mundos de prueba y demo.
testFloor :: Platform
testFloor = platform (position (-200) 0) 400 8

-- | Arma un 'World' de test con meta de run coherente (spawn, vidas, fase).
mkTestWorld :: Position -> Player -> [Enemy] -> [Platform] -> World
mkTestWorld spawn player enemies platforms =
  World
    { worldPlayer = player
    , worldEnemies = enemies
    , worldPlatforms = platforms
    , worldSpawnPoint = spawn
    , worldLives = defaultInitialLives
    , worldPhase = Playing
    }

{- | Mundo inicial para tests de física del jugador: jugador sobre el suelo, sin enemigos.

El jugador spawnea en @y = 80@ para que la gravedad lo baje hasta el suelo
en tests y demos.
-}
initialWorld :: World
initialWorld =
  let spawn = position 0 80
   in mkTestWorld spawn (spawnPlayer spawn) [] [testFloor]
