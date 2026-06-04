{- | Orquestación del ciclo de actualización del juego.

'updateGame' es el punto de entrada para un frame de simulación.
Lee la configuración con 'MonadReader', modifica el estado del mundo con 'MonadState',
y delega la simulación pura a 'Domain.Logic.Step.step'.
-}
module UseCases.UpdateGame (
  -- * Ciclo de update
  updateGame,
)
where

import Control.Monad.Reader (ask)
import Control.Monad.State (modify)

import Domain.Logic.Step (step)
import Domain.ValueObjects.DeltaTime (DeltaTime)
import Domain.ValueObjects.Input (Input)
import UseCases.GameMonad (GameM, physicsParamsFromConfig)

{- | Actualiza el estado del mundo para un frame dado.

Secuencia (dentro de 'GameM'):

  1. Lee 'GameConfig' con 'ask'.
  2. Aplica 'step' (input, gravedad, integración, colisiones AABB) en el dominio puro.
-}
updateGame :: DeltaTime -> Input -> GameM ()
updateGame dt input = do
  cfg <- ask
  modify (step (physicsParamsFromConfig cfg) dt input)
