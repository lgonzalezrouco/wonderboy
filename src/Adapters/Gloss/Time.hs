-- | Adaptador de tiempo: segundos de Gloss → 'DeltaTime' acotado.
module Adapters.Gloss.Time (
  capDeltaTime,
)
where

import Adapters.Gloss.Config (maxDeltaSeconds)
import Domain.ValueObjects.DeltaTime (DeltaTime, deltaTime)

-- | Convierte segundos transcurridos de Gloss a 'DeltaTime', con tope en 'maxDeltaSeconds'.
capDeltaTime :: Float -> DeltaTime
capDeltaTime secs = deltaTime (min secs maxDeltaSeconds)
