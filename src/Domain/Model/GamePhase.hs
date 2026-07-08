module Domain.Model.GamePhase (
  GamePhase (..),
  isSimulationFrozen,
)
where

import GHC.Generics (Generic)

data GamePhase
  = Playing
  | LevelComplete
  | GameOver
  | Victory
  deriving (Eq, Show, Generic)

isSimulationFrozen :: GamePhase -> Bool
isSimulationFrozen Playing = False
isSimulationFrozen _ = True
