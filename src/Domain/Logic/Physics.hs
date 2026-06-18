{- | Física cinemática del jugador y enemigos (sin colisiones).

Colisiones con plataformas en @Domain.Logic.Collision@.
-}
module Domain.Logic.Physics (
  applyHorizontalInput,
  applyGravity,
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

-- | Traduce input horizontal en @vx@ (sin salto; ver 'applyJump').
applyHorizontalInput :: PhysicsParams -> Input -> Player -> Player
applyHorizontalInput pp input p =
  p{playerVel = velocity (inputHorizontalSign input * ppMoveSpeed pp) (velY (playerVel p))}

{- | Impulso de salto tras gravedad, si hubo press de salto y el jugador estaba en el suelo al inicio del frame.

'inputJump' debe ser 'True' solo en el frame del press (ver 'Domain.ValueObjects.Input').
@wasOnGround@ es @playerOnGround@ antes de cualquier actualización del frame.
-}
applyJump :: PhysicsParams -> Input -> Bool -> Player -> Player
applyJump pp input wasOnGround p =
  if inputJump input && wasOnGround
    then p{playerVel = velocity (velX (playerVel p)) (ppJumpSpeed pp)}
    else p

-- | Aplica gravedad sobre la componente vertical: @vy' = vy - g * dt@.
applyGravity :: PhysicsParams -> DeltaTime -> Player -> Player
applyGravity pp dt p =
  p{playerVel = velocity (velX (playerVel p)) vy'}
 where
  t = seconds dt
  vy' = velY (playerVel p) - ppGravity pp * t

-- | Integra posición del jugador: @pos += vel * dt@.
integratePlayer :: DeltaTime -> Player -> Player
integratePlayer dt p =
  p{playerPos = integratePos (playerPos p) (playerVel p) dt}

-- | Integra posición de todos los enemigos (cinemática; sin gravedad ni colisión M6).
integrateEnemies :: DeltaTime -> [Enemy] -> [Enemy]
integrateEnemies dt = map (integrateEnemy dt)

integrateEnemy :: DeltaTime -> Enemy -> Enemy
integrateEnemy dt e =
  e{enemyPos = integratePos (enemyPos e) (enemyVel e) dt}

integratePos :: Position -> Velocity -> DeltaTime -> Position
integratePos pos vel dt =
  let t = seconds dt
   in translate (velX vel * t) (velY vel * t) pos
