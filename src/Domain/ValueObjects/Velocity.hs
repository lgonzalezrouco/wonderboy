{- | Velocidad 2D (vx, vy) de una entidad del juego, en píxeles por segundo.

Tipo separado de 'Position' para que el compilador no confunda ambos.
-}
module Domain.ValueObjects.Velocity (
  Velocity (..),
  velocity,
  velX,
  velY,
)
where

import GHC.Generics (Generic)

{- | Par de componentes (vx, vy) en píxeles por segundo.

Convención de signos: @vx > 0@ derecha, @vy > 0@ arriba (eje Y positivo hacia
arriba); la gravedad resta de @vy@ en cada frame.
-}
newtype Velocity = Velocity (Float, Float)
  deriving (Eq, Show, Generic)

velocity :: Float -> Float -> Velocity
velocity vx vy = Velocity (vx, vy)

velX :: Velocity -> Float
velX (Velocity (vx, _)) = vx

velY :: Velocity -> Float
velY (Velocity (_, vy)) = vy
