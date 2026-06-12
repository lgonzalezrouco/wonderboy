{- | Puerto de tiempo de simulación.

Abstrae la lectura del intervalo entre frames como 'DeltaTime'. Políticas
de timestep fijo o acumulación viven en el game loop (M8), no en este port.
-}
module UseCases.Ports.TimePort (
  -- * Puerto
  TimePort (..),
)
where

import Domain.ValueObjects.DeltaTime (DeltaTime)

-- | Capacidad de consultar el delta time del frame actual.
class Monad m => TimePort m where
  pollDeltaTime :: m DeltaTime
