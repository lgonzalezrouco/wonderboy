{- | Fase de la partida en curso (run-wide).

M9 introduce 'Playing' y 'GameOver'; M18 añade transiciones de nivel y victoria.
-}
module Domain.Model.GamePhase (
  GamePhase (..),
)
where

import GHC.Generics (Generic)

-- | Estado de flujo de la partida actual.
data GamePhase
  = Playing
  | GameOver
  deriving (Eq, Show, Generic)
