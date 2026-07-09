module Domain.ValueObjects.Lives (
  Lives,
  lives,
  livesCount,
  noLives,
  loseLife,
)
where

import GHC.Generics (Generic)

newtype Lives = Lives Int
  deriving (Eq, Ord, Show, Generic)

lives :: Int -> Lives
lives n = Lives (max 0 n)

livesCount :: Lives -> Int
livesCount (Lives n) = n

noLives :: Lives
noLives = Lives 0

loseLife :: Lives -> Lives
loseLife (Lives n) = Lives (max 0 (n - 1))
