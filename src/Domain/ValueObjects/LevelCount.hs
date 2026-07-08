{- | Cantidad de niveles en un run (catálogo de progresión).

El demo usa tres; el motor acepta cualquier valor >= 1 según el catálogo cargado.
-}
module Domain.ValueObjects.LevelCount (
  LevelCount,
  levelCount,
  levelCountPoints,
  isFinalLevel,
)
where

import GHC.Generics (Generic)

-- | Niveles en el run actual (siempre >= 1).
newtype LevelCount = LevelCount Int
  deriving (Eq, Show, Generic)

-- | Construye 'LevelCount', saturando en 1.
levelCount :: Int -> LevelCount
levelCount n = LevelCount (max 1 n)

levelCountPoints :: LevelCount -> Int
levelCountPoints (LevelCount n) = n

-- | @levelIndex@ es 1-based; 'True' si completar este nivel termina el run.
isFinalLevel :: Int -> LevelCount -> Bool
isFinalLevel idx lc = idx >= levelCountPoints lc
