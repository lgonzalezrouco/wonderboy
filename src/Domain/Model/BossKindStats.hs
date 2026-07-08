module Domain.Model.BossKindStats (
  BossGolemStats (..),
  BossBatStats (..),
  bossGolemStats,
  bossBatStats,
)
where

import Domain.ValueObjects.Health (Health, health)

data BossGolemStats = BossGolemStats
  { bgsWidth :: Float
  , bgsHeight :: Float
  , bgsMaxHealth :: Health
  }

data BossBatStats = BossBatStats
  { bbsWidth :: Float
  , bbsHeight :: Float
  , bbsMaxHealth :: Health
  }

bossGolemStats :: BossGolemStats
bossGolemStats =
  BossGolemStats
    { bgsWidth = 48
    , bgsHeight = 48
    , bgsMaxHealth = health 20
    }

bossBatStats :: BossBatStats
bossBatStats =
  BossBatStats
    { bbsWidth = 28
    , bbsHeight = 28
    , bbsMaxHealth = health 4
    }
