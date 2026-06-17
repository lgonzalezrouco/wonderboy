{- | Puntuación por nivel acumulada de pickups (también el umbral 'minScore').

Value object con invariante: nunca negativa. La suma de puntuaciones es su
'Semigroup' / 'Monoid' (mismo significado: acumular puntos), de modo que el delta
de un frame es @foldMap pickupValue@ sin recurrir a 'Int' crudo.
-}
module Domain.ValueObjects.Score (
  Score,
  score,
  scorePoints,
)
where

import GHC.Generics (Generic)

-- | Puntos (>= 0).
newtype Score = Score Int
  deriving (Eq, Ord, Show, Generic)

-- | Acumular puntos.
instance Semigroup Score where
  Score a <> Score b = Score (a + b)

instance Monoid Score where
  mempty = Score 0

-- | Construye 'Score', saturando en 0.
score :: Int -> Score
score n = Score (max 0 n)

-- | Puntos como 'Int'.
scorePoints :: Score -> Int
scorePoints (Score n) = n
