{- | Definiciones de niveles de demo (contenido autoral, no estado de runtime).

Mantiene 'Domain.Model.World' como una "fotografía" pura sin conocer el
catálogo de comportamientos: el cableado de presets de patrulla vive aquí.
-}
module Domain.DemoLevels (
  demoWorld,
)
where

import Domain.Logic.EntityBehaviours (patrolHorizontal)
import Domain.Model.Enemy (mkEnemy)
import Domain.Model.World (World (..), initialWorld)
import Domain.ValueObjects.Position (position)

{- | Mundo de demo M6: mismo layout que 'initialWorld' más un enemigo de patrulla.

Usado por @app/Main.hs@ y tests de comportamiento integrado.
-}
demoWorld :: World
demoWorld =
  initialWorld
    { worldEnemies =
        [mkEnemy 1 (position 50 8) (patrolHorizontal 40 90)]
    }
