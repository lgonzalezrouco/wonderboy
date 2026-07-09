module Domain.ValueObjects.Position (
  Position (..),
  position,
  posX,
  posY,
  positionBelowY,
  translate,
)
where

import GHC.Generics (Generic)

-- | Un punto 2D (x, y) en píxeles lógicos. +x a la derecha, +y hacia arriba.
newtype Position = Position (Float, Float)
  deriving (Eq, Show, Generic)

position :: Float -> Float -> Position
position x y = Position (x, y)

posX :: Position -> Float
posX (Position (x, _)) = x

posY :: Position -> Float
posY (Position (_, y)) = y

positionBelowY :: Float -> Position -> Bool
positionBelowY yThreshold (Position (_, y)) = y < yThreshold

translate :: Float -> Float -> Position -> Position
translate dx dy (Position (x, y)) = position (x + dx) (y + dy)
