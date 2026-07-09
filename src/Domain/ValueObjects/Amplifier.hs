module Domain.ValueObjects.Amplifier (
  Amplifier,
  mkAmplifier,
  identityAmplifier,
  unAmplifier,
)
where

import GHC.Generics (Generic)

-- | Factor de escala con piso en 1.0: solo fortalece (p. ej. alcance del enemigo, resistencia), nunca debilita.
newtype Amplifier = Amplifier Float
  deriving (Eq, Ord, Show, Generic)

minAmplifier, maxAmplifier :: Float
minAmplifier = 1.0
maxAmplifier = 3.0

identityAmplifier :: Amplifier
identityAmplifier = Amplifier 1.0

-- | Acota a [1.0, 3.0]. Un input NaN o infinito cae en la identidad, porque las comparaciones con NaN fallan.
mkAmplifier :: Float -> Amplifier
mkAmplifier x
  | isNaN x || isInfinite x = identityAmplifier
  | otherwise = Amplifier (max minAmplifier (min maxAmplifier x))

unAmplifier :: Amplifier -> Float
unAmplifier (Amplifier x) = x
