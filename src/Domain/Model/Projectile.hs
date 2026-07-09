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

data ProjectileMotion
  = Ballistic -- arco por gravedad, desaparece al chocar con una plataforma (tiros del jugador)
  | Linear -- velocidad constante, sin gravedad (disparos del enemigo)
  deriving (Eq, Show, Generic)

data ProjectileOwner
  = PlayerProjectile
  | EnemyProjectile
  deriving (Eq, Show, Generic)

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

projectileAabb :: Projectile -> Aabb
projectileAabb p =
  aabbFromBottomCenter
    (projectilePos p)
    (projectileWidth p)
    (projectileHeight p)

{- | Genera un tiro del jugador desde la posición dada en la dirección 'Facing'. Los
Floats son la velocidad horizontal y el empuje hacia arriba (px/s), luego el ancho y alto de la caja (px).
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

{- | Genera un disparo del enemigo dirigido hacia (dx, dy) (no hace falta normalizarlo). 'speed'
(px/s) fija la magnitud, luego el lifetime y el ancho y alto de la caja (px).
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
