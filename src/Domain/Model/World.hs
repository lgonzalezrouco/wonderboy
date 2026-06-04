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
)
where

import GHC.Generics (Generic)

import Domain.Model.Enemy (Enemy (..))
import Domain.Model.Platform (Platform, platform)
import Domain.Model.Player (Player (..), spawnPlayer)
import Domain.ValueObjects.Position (position)

-- | Estado completo de la simulación.
data World = World
  { worldPlayer :: Player
  , worldEnemies :: [Enemy]
  , worldPlatforms :: [Platform]
  }
  deriving (Eq, Show, Generic)

{- | Mundo inicial: jugador sobre el origen del nivel, suelo de prueba, sin enemigos.

El jugador spawnea en @y = 80@ para que la gravedad lo baje hasta el suelo
en el demo de @app/Main.hs@.
-}
initialWorld :: World
initialWorld =
  World
    { worldPlayer = spawnPlayer (position 0 80)
    , worldEnemies = []
    , worldPlatforms =
        [ platform (position (-200) 0) 400 8
        ]
    }
