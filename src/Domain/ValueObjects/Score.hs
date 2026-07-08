module Domain.ValueObjects.Score (
  Score,
  score,
  scorePoints,
)
where

import GHC.Generics (Generic)

newtype Score = Score Int
  deriving (Eq, Ord, Show, Generic)

instance Semigroup Score where
  Score a <> Score b = Score (a + b)

instance Monoid Score where
  mempty = Score 0

score :: Int -> Score
score n = Score (max 0 n)

scorePoints :: Score -> Int
scorePoints (Score n) = n
