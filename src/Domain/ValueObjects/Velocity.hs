{-# LANGUAGE DerivingStrategies #-}

-- | Velocidad 2D de una entidad del juego.
module Domain.ValueObjects.Velocity
  ( Velocity (..)
  , velocity
  , velX
  , velY
  )
where

import GHC.Generics (Generic)

-- | Par de componentes de velocidad (vx, vy) en píxeles por segundo.
newtype Velocity = Velocity (Float, Float)
  deriving stock (Eq, Show, Generic)

-- | Construye una 'Velocity' a partir de sus componentes.
velocity :: Float -> Float -> Velocity
velocity vx vy = Velocity (vx, vy)

-- | Componente horizontal de la velocidad.
velX :: Velocity -> Float
velX (Velocity (vx, _)) = vx

-- | Componente vertical de la velocidad.
velY :: Velocity -> Float
velY (Velocity (_, vy)) = vy
