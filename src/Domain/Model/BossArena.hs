{- | Arena de jefe: límites horizontales opcionales mientras el jefe vive.

Los bordes @left@ y @right@ son las aristas interiores jugables en X (la caja
del jugador no puede cruzarlas con el jefe vivo). La serialización JSON vive en
@UseCases.Serialization.LevelCodec@, no aquí.
-}
module Domain.Model.BossArena (
  BossArena (..),
  BossArenaDef (..),
  mkBossArena,
)
where

import GHC.Generics (Generic)

-- | Límites interiores jugables en X (runtime).
data BossArena = BossArena
  { bossArenaLeft :: Float
  , bossArenaRight :: Float
  }
  deriving (Eq, Show, Generic)

-- | Definición autoral en JSON del nivel.
data BossArenaDef = BossArenaDef
  { bossArenaDefLeft :: Float
  , bossArenaDefRight :: Float
  }
  deriving (Eq, Show, Generic)

-- | Construye arena cuando @left < right@.
mkBossArena :: BossArenaDef -> Maybe BossArena
mkBossArena (BossArenaDef l r)
  | l < r = Just BossArena{bossArenaLeft = l, bossArenaRight = r}
  | otherwise = Nothing
