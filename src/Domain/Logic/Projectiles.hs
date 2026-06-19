-- | Lanzamiento, física y colisiones de proyectiles (puro).
module Domain.Logic.Projectiles (
  resolveProjectiles,
)
where

import Data.List (find)

import Domain.Logic.MovingPlatforms (allCollisionPlatforms)
import Domain.Logic.PlayerLife (applyDamage)
import Domain.Model.Enemy (Enemy (..), enemyAabb, enemyHealth, enemyId, enemyShootCooldownFrames)
import Domain.Model.Platform (Platform, platformAabb)
import Domain.Model.Player (
  Player (..),
  playerAabb,
  playerFacing,
  playerInvincibilityFrames,
  playerThrowCooldownFrames,
 )
import Domain.Model.Projectile (
  Projectile (..),
  ProjectileMotion (..),
  ProjectileOwner (..),
  projectileAabb,
  spawnPlayerProjectile,
 )
import Domain.Model.World (World (..))
import Domain.ValueObjects.Aabb (
  Aabb (..),
  aabbMaxX,
  aabbMinX,
  aabbMinY,
  aabbOverlaps,
 )
import Domain.ValueObjects.CombatParams (CombatParams (..))
import Domain.ValueObjects.DeltaTime (DeltaTime, seconds)
import Domain.ValueObjects.Facing (Facing (..))
import Domain.ValueObjects.Frames (hasFramesLeft, tickFrames)
import Domain.ValueObjects.Health (isDepleted, reduceHealth)
import Domain.ValueObjects.Input (Input (..))
import Domain.ValueObjects.PhysicsParams (PhysicsParams (..))
import Domain.ValueObjects.Position (Position, position, translate)
import Domain.ValueObjects.ThrowParams (ThrowParams (..))
import Domain.ValueObjects.Velocity (velX, velY, velocity)

-- | Spawn, avance, impactos y cooldown de proyectiles en un frame.
resolveProjectiles ::
  ThrowParams ->
  CombatParams ->
  PhysicsParams ->
  DeltaTime ->
  Input ->
  World ->
  World
resolveProjectiles tp cp pp dt input w =
  let w0 = trySpawn tp input w
      w0' = w0{worldEnemies = map tickEnemyShootCooldown (worldEnemies w0)}
      plats = allCollisionPlatforms (worldPlatforms w0') (worldMovingPlatforms w0')
      (survivors, removedPassive) =
        foldr
          (despawnPassive plats . advanceProjectile pp dt)
          ([], False)
          (worldProjectiles w0')
      w1 = w0'{worldProjectiles = survivors}
      (w2, finalProjectiles, removedOnHit) = resolveHits tp cp w1
   in w2
        { worldPlayer = applyCooldown tp (removedOnHit || removedPassive) (worldPlayer w2)
        , worldProjectiles = finalProjectiles
        }

tickEnemyShootCooldown :: Enemy -> Enemy
tickEnemyShootCooldown e
  | hasFramesLeft (enemyShootCooldownFrames e) =
      e{enemyShootCooldownFrames = tickFrames (enemyShootCooldownFrames e)}
  | otherwise =
      e

trySpawn :: ThrowParams -> Input -> World -> World
trySpawn tp input w
  | not (inputThrow input) = w
  | hasFramesLeft (playerThrowCooldownFrames (worldPlayer w)) = w
  | any isPlayerOwned (worldProjectiles w) = w
  | otherwise =
      let p = worldPlayer w
          body = playerAabb p
          spawnPos = throwSpawnPos body (playerFacing p) (tpWidth tp) (tpHeight tp)
          pid = worldNextProjectileId w
          proj = spawnFromPlayer tp pid spawnPos (playerFacing p)
       in w
            { worldProjectiles = worldProjectiles w ++ [proj]
            , worldNextProjectileId = pid + 1
            }

spawnFromPlayer :: ThrowParams -> Int -> Position -> Facing -> Projectile
spawnFromPlayer tp pid pos facing =
  spawnPlayerProjectile
    pid
    pos
    facing
    (tpHorizontalSpeed tp)
    (tpLiftSpeed tp)
    (tpLifetime tp)
    (tpWidth tp)
    (tpHeight tp)

throwSpawnPos :: Aabb -> Facing -> Float -> Float -> Position
throwSpawnPos body facing width height =
  let centerY = aabbMinY body + height * 0.5 + 8
   in case facing of
        FacingRight ->
          position (aabbMaxX body + width * 0.5) centerY
        FacingLeft ->
          position (aabbMinX body - width * 0.5) centerY

isPlayerOwned :: Projectile -> Bool
isPlayerOwned proj = projectileOwner proj == PlayerProjectile

isEnemyOwned :: Projectile -> Bool
isEnemyOwned proj = projectileOwner proj == EnemyProjectile

advanceProjectile :: PhysicsParams -> DeltaTime -> Projectile -> Projectile
advanceProjectile pp dt proj =
  case projectileMotion proj of
    Linear -> integrateProjectile dt (tickLifetime proj)
    Ballistic ->
      integrateProjectile dt (applyGravity pp dt (tickLifetime proj))

tickLifetime :: Projectile -> Projectile
tickLifetime proj =
  proj{projectileLifetime = tickFrames (projectileLifetime proj)}

applyGravity :: PhysicsParams -> DeltaTime -> Projectile -> Projectile
applyGravity pp dt proj =
  let t = seconds dt
      vy' = velY (projectileVel proj) - ppGravity pp * t
   in proj{projectileVel = velocity (velX (projectileVel proj)) vy'}

integrateProjectile :: DeltaTime -> Projectile -> Projectile
integrateProjectile dt proj =
  let t = seconds dt
      v = projectileVel proj
      dx = velX v * t
      dy = velY v * t
   in proj{projectilePos = translate dx dy (projectilePos proj)}

despawnPassive :: [Platform] -> Projectile -> ([Projectile], Bool) -> ([Projectile], Bool)
despawnPassive plats proj (survivors, removedPlayer) =
  let landed =
        projectileMotion proj == Ballistic
          && any (aabbOverlaps (projectileAabb proj) . platformAabb) plats
      expired = not (hasFramesLeft (projectileLifetime proj))
   in if landed || expired
        then (survivors, removedPlayer || isPlayerOwned proj)
        else (proj : survivors, removedPlayer)

resolveHits ::
  ThrowParams ->
  CombatParams ->
  World ->
  (World, [Projectile], Bool)
resolveHits tp cp w =
  let (player', enemies', flying, removed) =
        foldl step (worldPlayer w, worldEnemies w, [], False) (worldProjectiles w)
      enemies'' = filter (not . isDepleted . enemyHealth) enemies'
   in (w{worldPlayer = player', worldEnemies = enemies''}, flying, removed)
 where
  step (player, enemies, flying, removed) proj =
    let box = projectileAabb proj
     in if isEnemyOwned proj && hitsPlayer box player
          then (damagePlayer cp player, enemies, flying, removed)
          else case find (aabbOverlaps box . enemyAabb) enemies of
            Just e ->
              let e' = e{enemyHealth = reduceHealth (tpDamage tp) (enemyHealth e)}
                  enemies' = map (\x -> if enemyId x == enemyId e then e' else x) enemies
               in (player, enemies', flying, removed || isPlayerOwned proj)
            Nothing ->
              (player, enemies, proj : flying, removed)

hitsPlayer :: Aabb -> Player -> Bool
hitsPlayer box player = aabbOverlaps box (playerAabb player)

damagePlayer :: CombatParams -> Player -> Player
damagePlayer cp player
  | hasFramesLeft (playerInvincibilityFrames player) = player
  | otherwise =
      applyDamage
        (cpContactDamage cp)
        player
          { playerInvincibilityFrames = cpInvincibilityDuration cp
          }

applyCooldown :: ThrowParams -> Bool -> Player -> Player
applyCooldown tp playerRemoved p =
  let ticked = tickThrowCooldown p
   in if playerRemoved
        then ticked{playerThrowCooldownFrames = tpCooldown tp}
        else ticked

tickThrowCooldown :: Player -> Player
tickThrowCooldown p
  | hasFramesLeft (playerThrowCooldownFrames p) =
      p{playerThrowCooldownFrames = tickFrames (playerThrowCooldownFrames p)}
  | otherwise =
      p
