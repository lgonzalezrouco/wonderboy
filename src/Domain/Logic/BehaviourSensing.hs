-- | Sensores puros para el intérprete del DSL de comportamiento (M13).
module Domain.Logic.BehaviourSensing (
  playerHorizontalDelta,
  playerHorizontalDistance,
  spawnHorizontalDelta,
  nearSpawnHorizontally,
  horizontalSign,
)
where

import Domain.Model.Enemy (Enemy (..))
import Domain.Model.Player (playerPos)
import Domain.Model.World (World (..))
import Domain.ValueObjects.Position (posX)

-- | Desplazamiento horizontal jugador − enemigo (positivo = jugador a la derecha).
playerHorizontalDelta :: World -> Enemy -> Float
playerHorizontalDelta w e =
  posX (playerPos (worldPlayer w)) - posX (enemyPos e)

-- | Distancia horizontal entre pies del jugador y del enemigo.
playerHorizontalDistance :: World -> Enemy -> Float
playerHorizontalDistance w e = abs (playerHorizontalDelta w e)

-- | Desplazamiento horizontal spawn anchor − enemigo.
spawnHorizontalDelta :: Enemy -> Float
spawnHorizontalDelta e = posX (enemySpawnPos e) - posX (enemyPos e)

-- | Verdadero si el enemigo está a menos de @radius@ px del spawn anchor (horizontal).
nearSpawnHorizontally :: Float -> Enemy -> Bool
nearSpawnHorizontally radius e = abs (spawnHorizontalDelta e) <= radius

-- | Signo horizontal con cero en coincidencia exacta.
horizontalSign :: Float -> Float
horizontalSign x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0
