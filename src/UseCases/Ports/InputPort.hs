{- | Puerto de entrada del jugador.

Abstrae la lectura de intención por frame hacia el value object de dominio
'Input'. El adaptador concreto (M8) convierte eventos del SO en esa forma
y detecta el press de salto; 'UseCases/' no ve teclado ni Gloss.
-}
module UseCases.Ports.InputPort (
  -- * Puerto
  InputPort (..),
)
where

import Domain.ValueObjects.Input (Input)

-- | Capacidad de consultar la intención del jugador en el frame actual.
class (Monad m) => InputPort m where
  pollInput :: m Input
