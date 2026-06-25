{- | Factor de escala validado para velocidad y otras perillas bidireccionales.
Construir solo vía 'mkMultiplier'.
-}
module Domain.ValueObjects.Multiplier (
  Multiplier,
  mkMultiplier,
  identityMultiplier,
  unMultiplier,
)
where

import GHC.Generics (Generic)

newtype Multiplier = Multiplier Float
  deriving (Eq, Ord, Show, Generic)

minMultiplier, maxMultiplier :: Float
minMultiplier = 0.3
maxMultiplier = 3.0

identityMultiplier :: Multiplier
identityMultiplier = Multiplier 1.0

-- | Clampea a rango; NaN/±∞ → identidad (las comparaciones con NaN fallan).
mkMultiplier :: Float -> Multiplier
mkMultiplier x
  | isNaN x || isInfinite x = identityMultiplier
  | otherwise = Multiplier (max minMultiplier (min maxMultiplier x))

unMultiplier :: Multiplier -> Float
unMultiplier (Multiplier x) = x
