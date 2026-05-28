{-# LANGUAGE DerivingStrategies #-}

-- | Coordenada 2D en el espacio del juego.
module Domain.ValueObjects.Position
  ( Position (..)
  , position
  , posX
  , posY
  )
where

import GHC.Generics (Generic)

-- | Par de coordenadas (x, y) en píxeles lógicos.
newtype Position = Position (Float, Float)
  deriving stock (Eq, Show, Generic)

-- | Construye una 'Position' a partir de sus componentes.
position :: Float -> Float -> Position
position x y = Position (x, y)

-- | Componente horizontal.
posX :: Position -> Float
posX (Position (x, _)) = x

-- | Componente vertical.
posY :: Position -> Float
posY (Position (_, y)) = y
