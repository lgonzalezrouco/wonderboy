{- | Fase gruesa del intento de nivel (simulación activa vs fin de partida).

M9 expone sólo 'Playing' y 'GameOver'; M18 añadirá level complete y victory.
-}
module Domain.Model.GamePhase (
  -- * Tipo
  GamePhase (..),
)
where

import GHC.Generics (Generic)

-- | Estado de la corrida en el mundo runtime.
data GamePhase
  = -- | La simulación avanza frame a frame.
    Playing
  | -- | Sin vidas restantes; la simulación queda congelada.
    GameOver
  deriving (Eq, Show, Generic)
