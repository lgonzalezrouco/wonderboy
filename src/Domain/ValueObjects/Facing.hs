{- | Orientación horizontal del jugador para alcance de melee.

Se actualiza desde la intención horizontal del frame; persiste cuando no hay
input lateral (grill M10).
-}
module Domain.ValueObjects.Facing (
  Facing (..),
  facingTowardHorizontal,
)
where

import GHC.Generics (Generic)

-- | Hacia qué lado mira el jugador en el plano del juego.
data Facing = FacingLeft | FacingRight
  deriving (Eq, Show, Generic)

{- | Orientación derivada de un desplazamiento horizontal @dx@.

Regla única de "mirar hacia donde se mueve": @dx > 0@ → 'FacingRight',
@dx < 0@ → 'FacingLeft', @dx == 0@ → mantiene el facing actual. La comparten el
combate del jugador y el intérprete del DSL de enemigos.
-}
facingTowardHorizontal :: Facing -> Float -> Facing
facingTowardHorizontal current dx = case compare dx 0 of
  GT -> FacingRight
  LT -> FacingLeft
  EQ -> current
