module Domain.ValueObjects.LifeParams (
  LifeParams (..),
  lifeParams,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Frames (Frames)
import Domain.ValueObjects.Health (Health)

data LifeParams = LifeParams
  { lpMaxHealth :: Health
  , lpDeathMargin :: Float
  -- ^ px por debajo de la plataforma más baja antes de que el jugador caiga fuera de los límites
  , lpRespawnInvincibilityFrames :: Frames
  }
  deriving (Eq, Show, Generic)

lifeParams :: Health -> Float -> Frames -> LifeParams
lifeParams maxHealth margin respawnInvincibility =
  LifeParams
    { lpMaxHealth = maxHealth
    , lpDeathMargin = margin
    , lpRespawnInvincibilityFrames = respawnInvincibility
    }
