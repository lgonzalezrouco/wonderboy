module Domain.ValueObjects.Facing (
  Facing (..),
  facingTowardHorizontal,
  facingScale,
)
where

import GHC.Generics (Generic)

data Facing = FacingLeft | FacingRight
  deriving (Eq, Show, Generic)

-- | Orientación derivada de un delta horizontal. dx == 0 mantiene la orientación actual (no se da vuelta al frenar).
facingTowardHorizontal :: Facing -> Float -> Facing
facingTowardHorizontal current dx = case compare dx 0 of
  GT -> FacingRight
  LT -> FacingLeft
  EQ -> current

facingScale :: Facing -> Float
facingScale FacingLeft = -1
facingScale FacingRight = 1
