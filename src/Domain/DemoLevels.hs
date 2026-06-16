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
import Domain.Model.World (World (..), initialWorld)
import Domain.ValueObjects.Position (position)

-- | Shuttle horizontal de demo (M12): recorre x ∈ [-60, 60] a y = 24.
demoShuttle :: MovingPlatform
demoShuttle =
  fromMaybe (error "demoShuttle: invalid moving platform") $
    mkMovingPlatform
      1
      (position (-60) 24)
      48
      8
      (position (-60) 24)
      (position 60 24)
      40
      True

{- | Mundo de demo M6: mismo layout que 'initialWorld' más un enemigo de patrulla.

Usado por @app/Main.hs@ y tests de comportamiento integrado.
-}
demoWorld :: World
demoWorld =
  initialWorld
    { worldEnemies =
        [mkEnemy 1 (position 50 8) (patrolHorizontal 40 90)]
    , worldPickups =
        catMaybes
          [ mkPickup 1 (position (-80) 8) 100
          , mkPickup 2 (position 120 8) 50
          , mkPickup 3 (position 0 40) 200
          ]
    , worldMovingPlatforms = [demoShuttle]
    , worldMinScore = 150
    }
