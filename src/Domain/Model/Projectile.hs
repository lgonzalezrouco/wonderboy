-- | Proyectiles de jugador y enemigo (entidades de corta duración).
module Domain.Model.Projectile (
  ProjectileMotion (..),
  ProjectileOwner (..),
  Projectile (..),
  projectileAabb,
  spawnPlayerProjectile,
  spawnEnemyProjectile,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Aabb (Aabb, aabbFromBottomCenter)
import Domain.ValueObjects.Facing (Facing (..))
import Domain.ValueObjects.Frames (Frames)
import Domain.ValueObjects.Position (Position)
import Domain.ValueObjects.Velocity (Velocity, velocity)

-- | Perfil de movimiento del proyectil.
data ProjectileMotion
  = -- | Gravedad + despawn al tocar plataforma (arco del jugador).
    Ballistic
  | -- | Velocidad constante (proyectiles enemigos).
    Linear
  deriving (Eq, Show, Generic)

-- | Quién disparó el proyectil.
data ProjectileOwner
  = PlayerProjectile
  | EnemyProjectile
  deriving (Eq, Show, Generic)

-- | Estado de un proyectil en un frame.
data Projectile = Projectile
  { projectileId :: Int
  , projectilePos :: Position
  , projectileVel :: Velocity
  , projectileLifetime :: Frames
  , projectileMotion :: ProjectileMotion
  , projectileOwner :: ProjectileOwner
  , projectileWidth :: Float
  , projectileHeight :: Float
  }
  deriving (Eq, Show, Generic)

-- | Caja de colisión del proyectil (centro inferior en 'projectilePos').
projectileAabb :: Projectile -> Aabb
projectileAabb p =
  aabbFromBottomCenter
    (projectilePos p)
    (projectileWidth p)
    (projectileHeight p)

{- | Crea un proyectil del jugador con velocidad inicial según 'Facing'.

La posición es el punto de spawn; el tamaño viene de 'ThrowParams'.
-}
spawnPlayerProjectile ::
  Int ->
  Position ->
  Facing ->
  Float ->
  Float ->
  Frames ->
  Float ->
  Float ->
  Projectile
spawnPlayerProjectile pid pos facing hSpeed liftSpeed lifetime width height =
  let (vx, vy) = case facing of
        FacingRight -> (hSpeed, liftSpeed)
        FacingLeft -> (-hSpeed, liftSpeed)
   in Projectile
        { projectileId = pid
        , projectilePos = pos
        , projectileVel = velocity vx vy
        , projectileLifetime = lifetime
        , projectileMotion = Ballistic
        , projectileOwner = PlayerProjectile
        , projectileWidth = width
        , projectileHeight = height
        }

{- | Crea un proyectil enemigo con velocidad inicial hacia @(dx, dy)@.

La posición es el punto de spawn; el vector no tiene que estar normalizado.
-}
spawnEnemyProjectile ::
  Int ->
  Position ->
  Float ->
  Float ->
  Float ->
  Frames ->
  Float ->
  Float ->
  Projectile
spawnEnemyProjectile pid pos dx dy speed lifetime width height =
  let dist = sqrt (dx * dx + dy * dy)
      scale = if dist <= 0 then 0 else speed / dist
      vx = dx * scale
      vy = dy * scale
   in Projectile
        { projectileId = pid
        , projectilePos = pos
        , projectileVel = velocity vx vy
        , projectileLifetime = lifetime
        , projectileMotion = Linear
        , projectileOwner = EnemyProjectile
        , projectileWidth = width
        , projectileHeight = height
        }
