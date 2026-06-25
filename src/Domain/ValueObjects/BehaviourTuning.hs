{- | Tres perillas de la IA: speed ('Multiplier', puede reducir), reach y toughness
('Amplifier', piso 1.0).
-}
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
  , tuningReach :: Amplifier
  , tuningToughness :: Amplifier
  }
  deriving (Eq, Show, Generic)

identityTuning :: BehaviourTuning
identityTuning =
  BehaviourTuning identityMultiplier identityAmplifier identityAmplifier
