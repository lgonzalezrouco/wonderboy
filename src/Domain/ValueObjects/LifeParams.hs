{- | Parámetros de vida y muerte inyectados en el dominio puro cada frame.

Evita que @Domain.Logic.PlayerLife@ importe @UseCases.GameMonad@:
'UpdateGame' construye este value object desde 'GameConfig'.
-}
module Domain.ValueObjects.LifeParams (
  LifeParams (..),
  lifeParams,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Frames (Frames)
import Domain.ValueObjects.Health (Health)

-- | Constantes de vida para un frame (salud máxima, margen de caída).
data LifeParams = LifeParams
  { lpMaxHealth :: Health
  -- ^ Salud tras spawn o respawn.
  , lpDeathMargin :: Float
  -- ^ Píxeles bajo la plataforma más baja antes de out-of-bounds.
  , lpRespawnInvincibilityFrames :: Frames
  -- ^ Frames de invencibilidad otorgados al respawn tras perder una vida (M10).
  }
  deriving (Eq, Show, Generic)

-- | Construye 'LifeParams' desde componentes sueltos.
lifeParams :: Health -> Float -> Frames -> LifeParams
lifeParams maxHealth margin respawnInvincibility =
  LifeParams
    { lpMaxHealth = maxHealth
    , lpDeathMargin = margin
    , lpRespawnInvincibilityFrames = respawnInvincibility
    }
