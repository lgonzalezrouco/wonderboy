module Domain.ValueObjects.DeltaTime (
  DeltaTime,
  deltaTime,
  seconds,
  isFrozen,
)
where

import GHC.Generics (Generic)

-- | Tiempo entre dos frames consecutivos, en segundos (nunca negativo).
newtype DeltaTime = DeltaTime Float
  deriving (Eq, Show, Generic)

deltaTime :: Float -> DeltaTime
deltaTime t = DeltaTime (max 0 t)

seconds :: DeltaTime -> Float
seconds (DeltaTime t) = t

-- | Verdadero cuando el frame está congelado: no pasa tiempo simulado, así que ninguna fase debería avanzar.
isFrozen :: DeltaTime -> Bool
isFrozen dt = seconds dt <= 0
