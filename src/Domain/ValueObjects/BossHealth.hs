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

-- | Salud actual y máxima del jefe vivo en el nivel.
data BossHealth = BossHealth
  { bossHealthCurrent :: Health
  -- ^ Salud restante este frame.
  , bossHealthMax :: Health
  -- ^ Salud máxima al spawnear (denominador de la barra).
  }
  deriving (Eq, Show, Generic)

-- | Construye el par mostrado en el HUD.
bossHealth :: Health -> Health -> BossHealth
bossHealth = BossHealth
