{- | Las tres perillas de "personalidad" que la IA ajusta sobre la base de la clase.

Cada perilla es un 'Multiplier' ya validado. 'identityTuning' representa "sin ajuste"
(la IA no opinó o falló): todo en 1.0, dejando los números base del arquetipo intactos.
-}
module Domain.ValueObjects.BehaviourTuning (
  BehaviourTuning (..),
  identityTuning,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Multiplier (Multiplier, identityMultiplier)

-- | Multiplicadores de gameplay derivados de la prosa del @behaviourHint@.
data BehaviourTuning = BehaviourTuning
  { tuningSpeed :: Multiplier
  -- ^ Escala todas las velocidades de movimiento (patrulla, persecución, retorno).
  , tuningReach :: Multiplier
  -- ^ Escala el alcance reactivo: detección y persecución; @shootRange@ del archer.
  , tuningToughness :: Multiplier
  -- ^ Escala la salud (aguante).
  }
  deriving (Eq, Show, Generic)

-- | Sin ajuste: las tres perillas en 1.0 (los números base del arquetipo).
identityTuning :: BehaviourTuning
identityTuning =
  BehaviourTuning identityMultiplier identityMultiplier identityMultiplier
