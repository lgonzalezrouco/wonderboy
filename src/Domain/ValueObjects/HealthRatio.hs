{- | Proporción de salud restante (0 < ratio ≤ 1) para umbrales de fase de jefe.

Evita confundir un umbral de fase con otros escalares @Float@ (alcance, velocidad).
-}
module Domain.ValueObjects.HealthRatio (
  HealthRatio,
  healthRatio,
  healthRatioValue,
  healthAtOrBelowRatio,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Health (Health, healthPoints)

-- | Umbral como fracción de la salud máxima (p. ej. 0.66 = 66 %).
newtype HealthRatio = HealthRatio Float
  deriving (Eq, Show, Generic)

-- | Construye un umbral válido: @0 < ratio ≤ 1@.
healthRatio :: Float -> Maybe HealthRatio
healthRatio r
  | r > 0, r <= 1 = Just (HealthRatio r)
  | otherwise = Nothing

-- | Valor numérico del umbral (solo para depuración o catálogo estático).
healthRatioValue :: HealthRatio -> Float
healthRatioValue (HealthRatio r) = r

-- | Verdadero si @current / max ≤ ratio@ (con @max > 0@).
healthAtOrBelowRatio :: Health -> Health -> HealthRatio -> Bool
healthAtOrBelowRatio current maxHp (HealthRatio ratio) =
  let maxPoints = healthPoints maxHp
      curPoints = healthPoints current
   in maxPoints > 0 && fromIntegral curPoints / fromIntegral maxPoints <= ratio
