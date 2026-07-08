module Domain.ValueObjects.BehaviourTuning (
  BehaviourTuning (..),
  identityTuning,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Amplifier (Amplifier, identityAmplifier)
import Domain.ValueObjects.Multiplier (Multiplier, identityMultiplier)

data BehaviourTuning = BehaviourTuning
  { tuningSpeed :: Multiplier
  -- ^ escala la velocidad de movimiento del enemigo (un Multiplier, así que también puede frenar por debajo de 1)
  , tuningReach :: Amplifier
  -- ^ escala el alcance de ataque del enemigo
  , tuningToughness :: Amplifier
  -- ^ escala la salud del enemigo
  }
  deriving (Eq, Show, Generic)

identityTuning :: BehaviourTuning
identityTuning =
  BehaviourTuning identityMultiplier identityAmplifier identityAmplifier
