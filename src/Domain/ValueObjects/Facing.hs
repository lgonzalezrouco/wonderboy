{- | Orientación horizontal del jugador para alcance de melee.

Se actualiza desde la intención horizontal del frame; persiste cuando no hay
input lateral.
-}
module Domain.ValueObjects.Facing (
  Facing (..),
  facingTowardHorizontal,
)
where

import GHC.Generics (Generic)

data Facing = FacingLeft | FacingRight
  deriving (Eq, Show, Generic)

{- | Orientación derivada de un desplazamiento horizontal @dx@: @dx == 0@ mantiene
el facing actual. La comparten el combate del jugador y el DSL de enemigos.
-}
facingTowardHorizontal :: Facing -> Float -> Facing
facingTowardHorizontal current dx = case compare dx 0 of
  GT -> FacingRight
  LT -> FacingLeft
  EQ -> current
