module Domain.ValueObjects.HealthRatio (
  HealthRatio,
  healthRatio,
  healthRatioValue,
  healthAtOrBelowRatio,
  maxHealthRatio,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Health (Health, healthPoints)

-- | Una fracción de la salud máxima, 0 < r <= 1. Se usa como umbrales de boss phase.
newtype HealthRatio = HealthRatio Float
  deriving (Eq, Show, Generic)

healthRatio :: Float -> Maybe HealthRatio
healthRatio r
  | r > 0, r <= 1 = Just (HealthRatio r)
  | otherwise = Nothing

healthRatioValue :: HealthRatio -> Float
healthRatioValue (HealthRatio r) = r

maxHealthRatio :: HealthRatio
maxHealthRatio = HealthRatio 1.0

-- | Verdadero cuando current / max <= el ratio. Falso cuando la salud máxima es 0.
healthAtOrBelowRatio :: Health -> Health -> HealthRatio -> Bool
healthAtOrBelowRatio current maxHp (HealthRatio ratio) =
  let maxPoints = healthPoints maxHp
      curPoints = healthPoints current
   in maxPoints > 0 && fromIntegral curPoints / fromIntegral maxPoints <= ratio
