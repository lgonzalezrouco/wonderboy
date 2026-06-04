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
  demoWorld,
)
where

import GHC.Generics (Generic)

import Domain.Logic.EntityBehaviours (patrolHorizontal)
import Domain.Model.Enemy (Enemy (..), mkEnemy)
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

-- | Plataforma de suelo compartida por mundos de prueba y demo.
testFloor :: Platform
testFloor = platform (position (-200) 0) 400 8

{- | Mundo inicial para tests de física del jugador: jugador sobre el suelo, sin enemigos.

El jugador spawnea en @y = 80@ para que la gravedad lo baje hasta el suelo
en tests y demos.
-}
initialWorld :: World
initialWorld =
  World
    { worldPlayer = spawnPlayer (position 0 80)
    , worldEnemies = []
    , worldPlatforms = [testFloor]
    }

{- | Mundo de demo M6: mismo layout que 'initialWorld' más un enemigo de patrulla.

Usado por @app/Main.hs@ y tests de comportamiento integrado.
-}
demoWorld :: World
demoWorld =
  initialWorld
    { worldEnemies =
        [mkEnemy 1 (position 50 8) (patrolHorizontal 40 90)]
    }
