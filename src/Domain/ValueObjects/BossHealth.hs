{- | Salud del jefe para la barra HUD: actual y máxima de la instancia.

Proyecta el par que el adaptador necesita sin exponer tuplas sueltas en 'GameView'.
-}
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
