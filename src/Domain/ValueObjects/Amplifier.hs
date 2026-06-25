{- | Factor de amplificación con piso 1.0: solo potencia (alcance, salud), nunca reduce.
Construir solo vía 'mkAmplifier'.
-}
module Domain.ValueObjects.Amplifier (
  Amplifier,
  mkAmplifier,
  identityAmplifier,
  unAmplifier,
)
where

import GHC.Generics (Generic)

newtype Amplifier = Amplifier Float
  deriving (Eq, Ord, Show, Generic)

minAmplifier, maxAmplifier :: Float
minAmplifier = 1.0
maxAmplifier = 3.0

identityAmplifier :: Amplifier
identityAmplifier = Amplifier 1.0

-- | Clampea a rango; NaN/±∞ → identidad (las comparaciones con NaN fallan).
mkAmplifier :: Float -> Amplifier
mkAmplifier x
  | isNaN x || isInfinite x = identityAmplifier
  | otherwise = Amplifier (max minAmplifier (min maxAmplifier x))

unAmplifier :: Amplifier -> Float
unAmplifier (Amplifier x) = x
