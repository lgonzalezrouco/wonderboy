{- | Definiciones de niveles de demo (contenido autoral, no estado de runtime).

Mantiene 'Domain.Model.World' como una "fotografía" pura sin conocer el
catálogo de comportamientos: el cableado de presets de patrulla vive aquí.
-}
module Domain.DemoLevels (
  demoWorld,
)
where

import Data.Maybe (catMaybes, fromMaybe)

import Domain.Logic.EntityBehaviours (patrolHorizontal)
import Domain.Model.Enemy (mkEnemy)
import Domain.Model.MovingPlatform (MovingPlatform, mkMovingPlatform)
import Domain.Model.Pickup (mkPickup)
import Domain.Model.Player (spawnPlayer)
import Domain.Model.World (World (..), defaultMaxHealth, initialWorld)
import Domain.ValueObjects.Position (Position, position)

-- | Spawn del demo: izquierda del escenario, lejos del enemigo y de la ruta del shuttle.
demoSpawn :: Position
demoSpawn = position (-100) 80

-- | Shuttle elevado: queda sobre la altura de cabeza del jugador caminando.
demoShuttle :: MovingPlatform
demoShuttle =
  fromMaybe (error "demoShuttle: invalid moving platform") $
    mkMovingPlatform
      1
      (position 30 72)
      48
      8
      (position 30 72)
      (position 90 72)
      35
      True

{- | Mundo de demo M6: mismo layout que 'initialWorld' más un enemigo de patrulla.

Usado por @app/Main.hs@ y tests de comportamiento integrado.
-}
demoWorld :: World
demoWorld =
  initialWorld
    { worldPlayer = spawnPlayer defaultMaxHealth demoSpawn
    , worldSpawnPoint = demoSpawn
    , worldEnemies =
        [mkEnemy 1 (position 160 8) (patrolHorizontal 40 90)]
    , worldPickups =
        catMaybes
          [ mkPickup 1 (position (-120) 8) 100
          , mkPickup 2 (position 10 8) 50
          , mkPickup 3 (position 60 80) 200
          ]
    , worldMovingPlatforms = [demoShuttle]
    , worldMinScore = 150
    }
