module Domain.Logic.RunBehaviour (
  runBehaviourStep,
  stepEnemyBehaviour,
  playerHorizontalDistance,
)
where

import Control.Monad.Free (Free (..))
import Data.List (mapAccumL)

import Domain.Model.Enemy (
  Enemy (..),
  enemyAabb,
  enemyFacing,
  enemyKind,
  enemyPos,
  enemyShootCooldownFrames,
 )
import Domain.Model.EnemyKind (
  EnemyKindStats (eksMotion),
  EnemyMotionStats (ArcherMotion),
  enemyKindStats,
 )
import Domain.Model.EntityBehaviour (
  BehaviourProgram (..),
  EntityAction (..),
 )
import Domain.Model.Player (playerPos)
import Domain.Model.Projectile (spawnEnemyProjectile)
import Domain.Model.World (World (..))
import Domain.ValueObjects.Aabb (aabbMaxX, aabbMinX)
import Domain.ValueObjects.Facing (Facing (..), facingTowardHorizontal)
import Domain.ValueObjects.Frames (frameCount, hasFramesLeft, tickFrames)
import Domain.ValueObjects.Position (Position, posX, posY, position)
import Domain.ValueObjects.Velocity (Velocity, velocity)

-- Hila el mundo por los enemigos de izquierda a derecha (mapAccumL) para que los
-- disparos de cada enemigo y el id de proyectil incrementado los vea el siguiente, así los ids quedan únicos.
runBehaviourStep :: World -> World
runBehaviourStep w =
  let (w', enemies') = mapAccumL stepEnemyBehaviour w (worldEnemies w)
   in w'{worldEnemies = enemies'}

stepEnemyBehaviour :: World -> Enemy -> (World, Enemy)
stepEnemyBehaviour w e =
  let (prog', w', e') = stepProgram w (enemyProgram e) e
   in (w', e'{enemyProgram = prog'})

-- Los condicionales se resuelven en el lugar y recursan. Cualquier otro nodo cede vía `next`,
-- así ocurre exactamente un efecto por enemigo por frame (un loop de puros condicionales giraría en vacío).
stepProgram ::
  World ->
  BehaviourProgram ->
  Enemy ->
  (BehaviourProgram, World, Enemy)
stepProgram w (BehaviourProgram prog) e =
  case prog of
    Pure () -> (BehaviourProgram (Pure ()), w, e)
    Free (SetVelocity vel next) ->
      (BehaviourProgram next, w, e{enemyVel = vel})
    Free (WaitFrames n next)
      | frameCount n > 1 ->
          ( BehaviourProgram (Free (WaitFrames (tickFrames n) next))
          , w
          , e
          )
      | otherwise ->
          (BehaviourProgram next, w, e)
    Free (IfPlayerWithinRange range thenBranch elseBranch _) ->
      let (branch, e') = stepBranch (playerHorizontalDistance w e <= range) thenBranch elseBranch e
       in stepProgram w branch e'
    Free (IfNearSpawn radius thenBranch elseBranch _) ->
      let (branch, e') = stepBranch (nearSpawnHorizontally radius e) thenBranch elseBranch e
       in stepProgram w branch e'
    Free (MoveTowardPlayer speed next) ->
      (BehaviourProgram next, w, moveHorizontallyToward (playerHorizontalDelta w e) speed e)
    Free (MoveTowardPlayer2D speed next) ->
      (BehaviourProgram next, w, chasePlayer w speed e)
    Free (MoveToward speed next) ->
      (BehaviourProgram next, w, chasePlayer w speed e)
    Free (MoveTowardSpawn speed next) ->
      (BehaviourProgram next, w, moveHorizontallyToward (spawnHorizontalDelta e) speed e)
    Free (MoveTowardSpawn2D speed next) ->
      ( BehaviourProgram next
      , w
      , moveToward2D (spawnHorizontalDelta e) (spawnVerticalDelta e) speed e
      )
    Free (FacePlayer next) ->
      ( BehaviourProgram next
      , w
      , e
          { enemyVel = velocity 0 0
          , enemyFacing = facingTowardHorizontal (enemyFacing e) (playerHorizontalDelta w e)
          }
      )
    Free (SetFacingTowardPlayer next) ->
      ( BehaviourProgram next
      , w
      , e{enemyFacing = facingTowardHorizontal (enemyFacing e) (playerHorizontalDelta w e)}
      )
    Free (Shoot next) ->
      let (w', e') = executeShoot w e
       in (BehaviourProgram next, w', e')

stepBranch ::
  Bool ->
  BehaviourProgram ->
  BehaviourProgram ->
  Enemy ->
  (BehaviourProgram, Enemy)
stepBranch cond thenBranch elseBranch e =
  (if cond then thenBranch else elseBranch, e)

moveHorizontallyToward :: Float -> Float -> Enemy -> Enemy
moveHorizontallyToward dx speed e =
  let dir = horizontalSign dx
   in e
        { enemyVel = velocity (dir * speed) 0
        , enemyFacing = facingTowardHorizontal (enemyFacing e) dx
        }

chasePlayer :: World -> Float -> Enemy -> Enemy
chasePlayer w speed e =
  moveToward2D
    (playerHorizontalDelta w e)
    (playerVerticalDelta w e)
    speed
    e

moveToward2D :: Float -> Float -> Float -> Enemy -> Enemy
moveToward2D dx dy speed e =
  e
    { enemyVel = velocityToward2D dx dy speed
    , enemyFacing = facingTowardHorizontal (enemyFacing e) dx
    }

executeShoot :: World -> Enemy -> (World, Enemy)
executeShoot w e
  | hasFramesLeft (enemyShootCooldownFrames e) = (w, e)
  | otherwise =
      case eksMotion (enemyKindStats (enemyKind e)) of
        ArcherMotion _ cooldown projSpeed lifetime width height ->
          let playerFoot = playerPos (worldPlayer w)
              spawnPos = shootSpawnPos e width height
              dx = posX playerFoot - posX spawnPos
              dy = posY playerFoot - posY spawnPos
              pid = worldNextProjectileId w
              proj =
                spawnEnemyProjectile
                  pid
                  spawnPos
                  dx
                  dy
                  projSpeed
                  lifetime
                  width
                  height
           in ( w
                  { worldProjectiles = worldProjectiles w ++ [proj]
                  , worldNextProjectileId = pid + 1
                  }
              , e{enemyShootCooldownFrames = cooldown}
              )
        _ -> (w, e)

shootSpawnPos :: Enemy -> Float -> Float -> Position
shootSpawnPos e width height =
  let body = enemyAabb e
      centerY = posY (enemyPos e) + height * 0.5
      offset = width * 0.5 + 4
   in case enemyFacing e of
        FacingRight -> position (aabbMaxX body + offset) centerY
        FacingLeft -> position (aabbMinX body - offset) centerY

playerHorizontalDistance :: World -> Enemy -> Float
playerHorizontalDistance w e = abs (playerHorizontalDelta w e)

playerHorizontalDelta :: World -> Enemy -> Float
playerHorizontalDelta w e =
  posX (playerPos (worldPlayer w)) - posX (enemyPos e)

playerVerticalDelta :: World -> Enemy -> Float
playerVerticalDelta w e =
  posY (playerPos (worldPlayer w)) - posY (enemyPos e)

spawnHorizontalDelta :: Enemy -> Float
spawnHorizontalDelta e = posX (enemySpawnPos e) - posX (enemyPos e)

spawnVerticalDelta :: Enemy -> Float
spawnVerticalDelta e = posY (enemySpawnPos e) - posY (enemyPos e)

nearSpawnHorizontally :: Float -> Enemy -> Bool
nearSpawnHorizontally radius e = abs (spawnHorizontalDelta e) <= radius

horizontalSign :: Float -> Float
horizontalSign x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

velocityToward2D :: Float -> Float -> Float -> Velocity
velocityToward2D dx dy speed =
  let dist = sqrt (dx * dx + dy * dy)
   in if dist <= 0
        then velocity 0 0
        else
          let scale = speed / dist
           in velocity (dx * scale) (dy * scale)
