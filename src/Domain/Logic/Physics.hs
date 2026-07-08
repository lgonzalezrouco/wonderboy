module Domain.Logic.Physics (
  applyHorizontalInput,
  applyGravity,
  applyEnemyGravity,
  applyJump,
  integratePlayer,
  integrateEnemy,
  integrateEnemies,
)
where

import Domain.Model.Enemy (Enemy (..))
import Domain.Model.Player (Player (..))
import Domain.ValueObjects.DeltaTime (DeltaTime, seconds)
import Domain.ValueObjects.Input (Input (..), inputHorizontalSign)
import Domain.ValueObjects.PhysicsParams (PhysicsParams (..))
import Domain.ValueObjects.Position (Position, translate)
import Domain.ValueObjects.Velocity (Velocity, velX, velY, velocity)

applyHorizontalInput :: PhysicsParams -> Input -> Player -> Player
applyHorizontalInput pp input p =
  p{playerVel = velocity (inputHorizontalSign input * ppMoveSpeed pp) (velY (playerVel p))}

applyJump :: PhysicsParams -> Input -> Bool -> Player -> Player
applyJump pp input wasOnGround p =
  if inputJump input && wasOnGround
    then p{playerVel = velocity (velX (playerVel p)) (ppJumpSpeed pp)}
    else p

applyGravity :: PhysicsParams -> DeltaTime -> Player -> Player
applyGravity pp dt p =
  p{playerVel = velocity (velX (playerVel p)) vy'}
 where
  t = seconds dt
  vy' = velY (playerVel p) - ppGravity pp * t

applyEnemyGravity :: PhysicsParams -> DeltaTime -> Enemy -> Enemy
applyEnemyGravity pp dt e =
  e{enemyVel = velocity (velX (enemyVel e)) vy'}
 where
  t = seconds dt
  vy' = velY (enemyVel e) - ppGravity pp * t

integratePlayer :: DeltaTime -> Player -> Player
integratePlayer dt p =
  p{playerPos = integratePos (playerPos p) (playerVel p) dt}

integrateEnemies :: DeltaTime -> [Enemy] -> [Enemy]
integrateEnemies dt = map (integrateEnemy dt)

integrateEnemy :: DeltaTime -> Enemy -> Enemy
integrateEnemy dt e =
  e{enemyPos = integratePos (enemyPos e) (enemyVel e) dt}

integratePos :: Position -> Velocity -> DeltaTime -> Position
integratePos pos vel dt =
  let t = seconds dt
   in translate (velX vel * t) (velY vel * t) pos
