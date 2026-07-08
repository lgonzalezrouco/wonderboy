module Domain.ValueObjects.LevelCount (
  LevelCount,
  levelCount,
  levelCountPoints,
  isFinalLevel,
)
where

import GHC.Generics (Generic)

newtype LevelCount = LevelCount Int
  deriving (Eq, Show, Generic)

levelCount :: Int -> LevelCount
levelCount n = LevelCount (max 1 n)

levelCountPoints :: LevelCount -> Int
levelCountPoints (LevelCount n) = n

-- | El índice de nivel arranca en 1. Verdadero cuando completar este nivel termina la partida.
isFinalLevel :: Int -> LevelCount -> Bool
isFinalLevel idx lc = idx >= levelCountPoints lc
