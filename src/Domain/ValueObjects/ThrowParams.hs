{- | Parámetros del arco lanzado por el jugador.

Proyectado desde 'GameConfig' en cada frame; mantiene 'Domain.Logic.Projectiles' puro.
-}
module Domain.ValueObjects.ThrowParams (
  ThrowParams (..),
  throwParams,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Damage (Damage)
import Domain.ValueObjects.Frames (Frames)

-- | Constantes de lanzamiento y proyectil para un frame.
data ThrowParams = ThrowParams
  { tpCooldown :: Frames
  -- ^ Frames de espera tras despawn del proyectil del jugador.
  , tpLifetime :: Frames
  , tpHorizontalSpeed :: Float
  -- ^ Velocidad horizontal de lanzamiento (px/s).
  , tpLiftSpeed :: Float
  -- ^ Impulso vertical inicial (px/s, hacia arriba).
  , tpWidth :: Float
  , tpHeight :: Float
  , tpDamage :: Damage
  }
  deriving (Eq, Show, Generic)

throwParams ::
  Frames ->
  Frames ->
  Float ->
  Float ->
  Float ->
  Float ->
  Damage ->
  ThrowParams
throwParams cooldown lifetime hSpeed liftSpeed width height dmg =
  ThrowParams
    { tpCooldown = cooldown
    , tpLifetime = lifetime
    , tpHorizontalSpeed = hSpeed
    , tpLiftSpeed = liftSpeed
    , tpWidth = width
    , tpHeight = height
    , tpDamage = dmg
    }
