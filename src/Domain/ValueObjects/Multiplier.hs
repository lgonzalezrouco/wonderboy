module Domain.ValueObjects.Multiplier (
  Multiplier,
  mkMultiplier,
  identityMultiplier,
  unMultiplier,
)
where

import GHC.Generics (Generic)

-- | Factor de escala validado para perillas bidireccionales como la velocidad. Puede bajar de 1 o subir por encima.
newtype Multiplier = Multiplier Float
  deriving (Eq, Ord, Show, Generic)

minMultiplier, maxMultiplier :: Float
minMultiplier = 0.3
maxMultiplier = 3.0

identityMultiplier :: Multiplier
identityMultiplier = Multiplier 1.0

-- | Acota a [0.3, 3.0]. Un input NaN o infinito cae en la identidad, porque las comparaciones con NaN fallan.
mkMultiplier :: Float -> Multiplier
mkMultiplier x
  | isNaN x || isInfinite x = identityMultiplier
  | otherwise = Multiplier (max minMultiplier (min maxMultiplier x))

unMultiplier :: Multiplier -> Float
unMultiplier (Multiplier x) = x
