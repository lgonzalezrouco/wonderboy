module Domain.ValueObjects.ThrowParams (
  ThrowParams (..),
  throwParams,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Damage (Damage)
import Domain.ValueObjects.Frames (Frames)

data ThrowParams = ThrowParams
  { tpCooldown :: Frames
  , tpLifetime :: Frames
  , tpHorizontalSpeed :: Float
  -- ^ px/s, velocidad de lanzamiento en la dirección a la que se mira
  , tpLiftSpeed :: Float
  -- ^ px/s hacia arriba, impulso vertical inicial
  , tpWidth :: Float
  -- ^ px, ancho de la hitbox del proyectil
  , tpHeight :: Float
  -- ^ px, alto de la hitbox del proyectil
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
