{- | Estado completo del mundo del juego en un instante dado.

'World' es el tipo que 'GameState' representará en @UseCases.GameMonad@.
Es la "fotografía" de la simulación: todo lo que el motor necesita para
calcular el siguiente frame.

En Milestone 3 se agrega la geometría del nivel (plataformas, AABB) y la
función 'step' en @Domain.Logic.Physics@ reemplaza el placeholder 'advance'.
-}
module Domain.Model.World (
  -- * Tipo
  World (..),

  -- * Construcción
  initialWorld,

  -- * Transformaciones
  mapPlayer,

  -- * Integración cinemática (placeholder M2 → M3)
  advance,
)
where

import GHC.Generics (Generic)

import Domain.Model.Enemy (Enemy (..))
import Domain.Model.Player (Player (..), spawnPlayer)
import Domain.ValueObjects.DeltaTime (DeltaTime, seconds)
import Domain.ValueObjects.Position (Position, posX, posY, position)
import Domain.ValueObjects.Velocity (Velocity, velX, velY)

{- | Estado completo de la simulación.

Plataformas y geometría del nivel se agregan en Milestone 3 junto con
@Domain.Logic.Collision@. Mantener el tipo pequeño en M2 facilita el
trabajo incremental sin arriesgar diseños apresurados.
-}
data World = World
  { worldPlayer :: Player
  -- ^ El único jugador del juego.
  , worldEnemies :: [Enemy]
  -- ^ Lista de enemigos activos en el nivel.
  --   El DSL (M6) y el cargador de niveles (M8) la populan.
  }
  deriving (Eq, Show, Generic)

{- | Mundo inicial: jugador spawneado en el origen, sin enemigos.

Punto de partida para el demo de @app/Main.hs@ y para los tests futuros (M5).
La posición de spawn concreta del jugador depende del nivel; aquí usamos
el origen (0, 0) como placeholder neutral.
-}
initialWorld :: World
initialWorld =
  World
    { worldPlayer = spawnPlayer (position 0 0)
    , worldEnemies = []
    }

{- | Aplica una transformación al jugador dentro del mundo.

__Por qué una función de orden superior y no acceso directo?__

Sin `mapPlayer`, actualizar el jugador desde @UseCases.UpdateGame@ requiere:

@
world { worldPlayer = f (worldPlayer world) }
@

Esto repite la actualización anidada en cada sitio. `mapPlayer` encapsula
ese patrón y hace el código de uso más declarativo:

@
modify (mapPlayer (applyInput speed input))
@

Es una aplicación concreta de __composición de funciones__: `modify` de @mtl@
transforma el estado completo; `mapPlayer` enfoca esa transformación en el jugador.
-}
mapPlayer :: (Player -> Player) -> World -> World
mapPlayer f w = w{worldPlayer = f (worldPlayer w)}

-- La actualización de record `w { worldPlayer = ... }` crea un nuevo 'World'
-- con todos los campos iguales a `w` excepto `worldPlayer`. No hay mutación.

{- | Integra la posición de cada entidad aplicando @pos += vel * dt@.

__PLACEHOLDER de Milestone 2__ — sin gravedad ni colisiones.

En Milestone 3, @Domain.Logic.Physics@ reemplaza esta función con:

  * Gravedad aplicada sobre `playerVel` (eje y).
  * Colisión AABB contra plataformas que actualiza `playerOnGround`.
  * La firma de `step :: DeltaTime -> Input -> World -> World` absorberá este rol.

Por ahora `advance` hace lo mínimo necesario para que el motor pueda demostrar
movimiento sin errores de compilación.
-}
advance :: DeltaTime -> World -> World
advance dt w =
  w
    { worldPlayer = integratePlayer dt (worldPlayer w)
    , worldEnemies = map (integrateEnemy dt) (worldEnemies w)
    }

-- ---------------------------------------------------------------------------
-- Helpers internos (no exportados)
-- ---------------------------------------------------------------------------

-- | Integra la posición del jugador: pos_nueva = pos + vel * dt.
integratePlayer :: DeltaTime -> Player -> Player
integratePlayer dt p =
  p{playerPos = integratePos (playerPos p) (playerVel p) dt}

-- | Integra la posición de un enemigo: pos_nueva = pos + vel * dt.
integrateEnemy :: DeltaTime -> Enemy -> Enemy
integrateEnemy dt e =
  e{enemyPos = integratePos (enemyPos e) (enemyVel e) dt}

{- | Ecuación de movimiento uniforme: nueva_pos = pos + vel * dt.

@
x' = x + vx * dt
y' = y + vy * dt
@

Es la integración de Euler de primer orden: suficiente para el motor en M2.
M3 puede reemplazarla por integración con gravedad (aceleración constante).
-}
integratePos :: Position -> Velocity -> DeltaTime -> Position
integratePos pos vel dt =
  let t = seconds dt
      x' = posX pos + velX vel * t
      y' = posY pos + velY vel * t
   in position x' y'

-- `let ... in ...` introduce ligaduras locales.
-- `t`, `x'`, `y'` son solo nombres para las subexpresiones (sin efectos).
-- El apóstrofo en `x'`, `y'` es una convención en Haskell para "versión nueva de x".
