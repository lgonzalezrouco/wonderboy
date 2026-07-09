module Domain.Model.BossArena (
  BossArena (..),
  BossArenaDef (..),
  mkBossArena,
)
where

import GHC.Generics (Generic)

data BossArena = BossArena
  { bossArenaLeft :: Float
  -- ^ Paredes internas izquierda/derecha en px del mundo. El jugador se mantiene entre ellas mientras el boss viva.
  , bossArenaRight :: Float
  }
  deriving (Eq, Show, Generic)

-- | Forma de autoría de 'BossArena', tal como se escribe en el archivo de nivel.
data BossArenaDef = BossArenaDef
  { bossArenaDefLeft :: Float
  , bossArenaDefRight :: Float
  }
  deriving (Eq, Show, Generic)

mkBossArena :: BossArenaDef -> Maybe BossArena
mkBossArena (BossArenaDef l r)
  | l < r = Just BossArena{bossArenaLeft = l, bossArenaRight = r}
  | otherwise = Nothing
