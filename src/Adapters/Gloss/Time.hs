module Adapters.Gloss.Time (
  capDeltaTime,
)
where

import Adapters.Gloss.Config (maxDeltaSeconds)
import Domain.ValueObjects.DeltaTime (DeltaTime, deltaTime)

capDeltaTime :: Float -> DeltaTime
capDeltaTime secs = deltaTime (min secs maxDeltaSeconds)
