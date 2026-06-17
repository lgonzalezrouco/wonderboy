{- | Velocidad 2D de una entidad del juego.

Representa el desplazamiento (vx, vy) en píxeles por segundo.
Se mantiene como tipo separado de 'Position' para que el compilador
rechace confundir los dos: sumar dos velocidades tiene sentido,
sumar dos posiciones generalmente no.
-}
module Domain.ValueObjects.Velocity (
  Velocity (..),
  velocity,
  velX,
  velY,
)
where

import GHC.Generics (Generic)

{- | Par de componentes de velocidad (vx, vy) en píxeles por segundo.

Diseño idéntico a 'Position': `newtype` sobre tupla para costo cero y
seguridad de tipos. Ver la documentación de 'Position' para la justificación
completa de `newtype` vs `data`.

Convención de signos (se establece aquí para consistencia en `Domain.Logic`):

  * vx > 0 → movimiento hacia la derecha.
  * vy > 0 → movimiento hacia arriba (eje Y positivo hacia arriba, como en matemáticas).
  * La gravedad restará de vy en cada frame de física.
-}
newtype Velocity = Velocity (Float, Float)
  deriving (Eq, Show, Generic)

-- | Construye una 'Velocity' a partir de sus componentes vx y vy.
velocity :: Float -> Float -> Velocity
velocity vx vy = Velocity (vx, vy)

{- | Componente horizontal de la velocidad.

vx > 0 → derecha, vx < 0 → izquierda, vx = 0 → quieto en x.
-}
velX :: Velocity -> Float
velX (Velocity (vx, _)) = vx

{- | Componente vertical de la velocidad.

vy > 0 → sube, vy < 0 → baja. La gravedad decrece este valor en cada frame.
-}
velY :: Velocity -> Float
velY (Velocity (_, vy)) = vy
