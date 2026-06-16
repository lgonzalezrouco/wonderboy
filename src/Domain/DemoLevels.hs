{- | Definiciones de niveles de demo (contenido autoral, no estado de runtime).

Mantiene 'Domain.Model.World' como una "fotografía" pura sin conocer el
catálogo de comportamientos: el cableado de presets vive aquí.
-}
module Domain.DemoLevels (
  demoWorld,
)
where

import Data.Maybe (catMaybes, fromMaybe)

import Domain.Logic.EntityBehaviours (defaultProgramForKind)
import Domain.Model.Enemy (Enemy, spawnEnemy)
import Domain.Model.EnemyKind (EnemyKind (..))
import Domain.Model.MovingPlatform (MovingPlatform, mkMovingPlatform)
import Domain.Model.Pickup (mkPickup)
import Domain.Model.Player (spawnPlayer)
import Domain.Model.World (World (..), defaultMaxHealth, initialWorld)
import Domain.ValueObjects.Position (Position, position)

-- | Spawn del demo: izquierda del escenario, lejos del enemigo y de la ruta del shuttle.
demoSpawn :: Position
demoSpawn = position (-100) 80

demoEnemy :: Int -> EnemyKind -> Position -> Enemy
demoEnemy eid kind pos = spawnEnemy eid kind pos (defaultProgramForKind kind)

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

{- | Mundo de demo M13: tres clases de enemigo sobre el suelo compartido.

Usado por @app/Main.hs@ y smoke manual.
-}
demoWorld :: World
demoWorld =
  initialWorld
    { worldPlayer = spawnPlayer defaultMaxHealth demoSpawn
    , worldSpawnPoint = demoSpawn
    , worldEnemies =
        [ demoEnemy 1 SnailKind (position 40 8)
        , demoEnemy 2 BatKind (position 80 8)
        , demoEnemy 3 GolemKind (position 170 8)
        ]
    , worldPickups =
        catMaybes
          [ mkPickup 1 (position (-120) 8) 100
          , mkPickup 2 (position 10 8) 50
          , mkPickup 3 (position 60 80) 200
          ]
    , worldMovingPlatforms = [demoShuttle]
    , worldMinScore = 150
    }
