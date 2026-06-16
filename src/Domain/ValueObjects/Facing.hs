{- | Orientación horizontal del jugador para alcance de melee.

Se actualiza desde la intención horizontal del frame; persiste cuando no hay
input lateral (grill M10).
-}
module Domain.ValueObjects.Facing (
  Facing (..),
)
where

import GHC.Generics (Generic)

-- | Hacia qué lado mira el jugador en el plano del juego.
data Facing = FacingLeft | FacingRight
  deriving (Eq, Show, Generic)
