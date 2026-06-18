-- | Sensores puros para el intérprete del DSL de comportamiento (M13).
module Domain.Logic.BehaviourSensing (
  playerHorizontalDelta,
  playerHorizontalDistance,
  playerVerticalDelta,
  spawnHorizontalDelta,
  spawnVerticalDelta,
  nearSpawnHorizontally,
  horizontalSign,
  velocityToward2D,
)
where

import Domain.Model.Enemy (Enemy (..))
import Domain.Model.Player (playerPos)
import Domain.Model.World (World (..))
import Domain.ValueObjects.Position (posX, posY)
import Domain.ValueObjects.Velocity (Velocity, velocity)

-- | Desplazamiento horizontal jugador − enemigo (positivo = jugador a la derecha).
playerHorizontalDelta :: World -> Enemy -> Float
playerHorizontalDelta w e =
  posX (playerPos (worldPlayer w)) - posX (enemyPos e)

-- | Distancia horizontal entre pies del jugador y del enemigo.
playerHorizontalDistance :: World -> Enemy -> Float
playerHorizontalDistance w e = abs (playerHorizontalDelta w e)

-- | Desplazamiento vertical jugador − enemigo (positivo = jugador arriba).
playerVerticalDelta :: World -> Enemy -> Float
playerVerticalDelta w e =
  posY (playerPos (worldPlayer w)) - posY (enemyPos e)

-- | Desplazamiento horizontal spawn anchor − enemigo.
spawnHorizontalDelta :: Enemy -> Float
spawnHorizontalDelta e = posX (enemySpawnPos e) - posX (enemyPos e)

-- | Desplazamiento vertical spawn anchor − enemigo.
spawnVerticalDelta :: Enemy -> Float
spawnVerticalDelta e = posY (enemySpawnPos e) - posY (enemyPos e)

-- | Verdadero si el enemigo está a menos de @radius@ px del spawn anchor (horizontal).
nearSpawnHorizontally :: Float -> Enemy -> Bool
nearSpawnHorizontally radius e = abs (spawnHorizontalDelta e) <= radius

-- | Signo horizontal con cero en coincidencia exacta.
horizontalSign :: Float -> Float
horizontalSign x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

{- | Velocidad hacia @(dx, dy)@ a @speed@ px/s; en reposo si el vector es nulo.

Usado por enemigos voladores que persiguen o regresan en dos dimensiones.
-}
velocityToward2D :: Float -> Float -> Float -> Velocity
velocityToward2D dx dy speed =
  let dist = sqrt (dx * dx + dy * dy)
   in if dist <= 0
        then velocity 0 0
        else
          let scale = speed / dist
           in velocity (dx * scale) (dy * scale)
