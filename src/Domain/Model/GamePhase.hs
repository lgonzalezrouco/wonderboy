{- | Fase de la partida en curso (run-wide).

M9 introduce 'Playing' y 'GameOver'; M17 añade 'LevelComplete' y 'Victory'.
-}
module Domain.Model.GamePhase (
  GamePhase (..),
  isSimulationFrozen,
)
where

import GHC.Generics (Generic)

-- | Estado de flujo de la partida actual.
data GamePhase
  = Playing
  | -- | Victoria híbrida del nivel; simulación congelada hasta confirmar.
    LevelComplete
  | GameOver
  | -- | Nivel final superado; simulación congelada hasta reiniciar o salir.
    Victory
  deriving (Eq, Show, Generic)

-- | 'True' cuando la simulación no avanza ('Playing' es la única fase activa).
isSimulationFrozen :: GamePhase -> Bool
isSimulationFrozen Playing = False
isSimulationFrozen _ = True
