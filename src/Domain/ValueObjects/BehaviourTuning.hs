{- | Las tres perillas de "personalidad" que la IA ajusta sobre la base de la clase.

'tuningSpeed' es un 'Multiplier' (puede reducir, hasta 0.3): un enemigo más lento se ve y
se juega distinto. 'tuningReach' y 'tuningToughness' son 'Amplifier' (piso 1.0, solo
potencian): bajar el alcance o la vida por debajo del base produce comportamiento
degenerado o nulo (ver el value object). 'identityTuning' representa "sin ajuste": todo en
1.0, dejando los números base del arquetipo intactos.
-}
module Domain.ValueObjects.BehaviourTuning (
  BehaviourTuning (..),
  identityTuning,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Amplifier (Amplifier, identityAmplifier)
import Domain.ValueObjects.Multiplier (Multiplier, identityMultiplier)

-- | Multiplicadores de gameplay derivados de la prosa del @behaviourHint@.
data BehaviourTuning = BehaviourTuning
  { tuningSpeed :: Multiplier
  -- ^ Escala todas las velocidades de movimiento (patrulla, persecución, retorno); [0.3, 3.0].
  , tuningReach :: Amplifier
  -- ^ Amplifica el alcance reactivo (detección, persecución, @shootRange@); [1.0, 3.0], nunca reduce.
  , tuningToughness :: Amplifier
  -- ^ Amplifica la salud (aguante); [1.0, 3.0], nunca por debajo del base.
  }
  deriving (Eq, Show, Generic)

-- | Sin ajuste: speed neutro y los dos amplificadores en su identidad (los números base).
identityTuning :: BehaviourTuning
identityTuning =
  BehaviourTuning identityMultiplier identityAmplifier identityAmplifier
