{- | Tiempo entre dos frames consecutivos, en segundos.

Invariante: nunca negativo. El constructor de datos está oculto; se usa el
smart constructor 'deltaTime'.
-}
module Domain.ValueObjects.DeltaTime (
  -- * Tipo
  DeltaTime,

  -- * Construcción
  deltaTime,

  -- * Acceso
  seconds,

  -- * Predicados
  isFrozen,
)
where

import GHC.Generics (Generic)

-- | Intervalo de tiempo entre frames, en segundos (>= 0 por construcción).
newtype DeltaTime = DeltaTime Float
  deriving (Eq, Show, Generic)

{- | Construye un 'DeltaTime' saturando en 0: un valor negativo (p. ej. por un
bug del adaptador de tiempo) no se propaga al motor de física.
-}
deltaTime :: Float -> DeltaTime
deltaTime t = DeltaTime (max 0 t)

-- | Valor en segundos de un 'DeltaTime' (>= 0 por construcción).
seconds :: DeltaTime -> Float
seconds (DeltaTime t) = t

{- | 'True' cuando el frame está congelado: no transcurre tiempo simulado y
ninguna fase debe avanzar. Como 'deltaTime' garantiza @seconds dt >= 0@, el
@<= 0@ equivale a @== 0@ pero es robusto ante valores degenerados.
-}
isFrozen :: DeltaTime -> Bool
isFrozen dt = seconds dt <= 0
