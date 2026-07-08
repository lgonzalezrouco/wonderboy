module Domain.ValueObjects.BossHealth (
  BossHealth (..),
  bossHealth,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Health (Health)

data BossHealth = BossHealth
  { bossHealthCurrent :: Health
  , bossHealthMax :: Health
  }
  deriving (Eq, Show, Generic)

bossHealth :: Health -> Health -> BossHealth
bossHealth = BossHealth
