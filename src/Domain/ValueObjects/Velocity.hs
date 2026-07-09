module Domain.ValueObjects.Velocity (
  Velocity (..),
  velocity,
  velX,
  velY,
)
where

import GHC.Generics (Generic)

-- | Velocidad 2D (vx, vy) en px/s. +x a la derecha, +y hacia arriba. La gravedad se resta de vy en cada frame.
newtype Velocity = Velocity (Float, Float)
  deriving (Eq, Show, Generic)

velocity :: Float -> Float -> Velocity
velocity vx vy = Velocity (vx, vy)

velX :: Velocity -> Float
velX (Velocity (vx, _)) = vx

velY :: Velocity -> Float
velY (Velocity (_, vy)) = vy
