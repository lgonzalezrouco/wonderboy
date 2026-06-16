-- | Sensores puros para el intérprete del DSL de comportamiento (M13).
module Domain.Logic.BehaviourSensing (
  playerHorizontalDistance,
  nearSpawnHorizontally,
  horizontalSign,
  facingTowardHorizontal,
)
where

import Domain.Model.Enemy (Enemy (..))
import Domain.Model.Player (playerPos)
import Domain.Model.World (World (..))
import Domain.ValueObjects.Facing (Facing (..))
import Domain.ValueObjects.Position (posX)

-- | Distancia horizontal entre pies del jugador y del enemigo.
playerHorizontalDistance :: World -> Enemy -> Float
playerHorizontalDistance w e =
  abs (posX (playerPos (worldPlayer w)) - posX (enemyPos e))

-- | Verdadero si el enemigo está a menos de @radius@ px del spawn anchor (horizontal).
nearSpawnHorizontally :: Float -> Enemy -> Bool
nearSpawnHorizontally radius e =
  abs (posX (enemyPos e) - posX (enemySpawnPos e)) <= radius

-- | Signo horizontal con cero en coincidencia exacta.
horizontalSign :: Float -> Float
horizontalSign x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

-- | Facing hacia un desplazamiento horizontal (mantiene facing si @dx == 0@).
facingTowardHorizontal :: Facing -> Float -> Facing
facingTowardHorizontal current dx = case horizontalSign dx of
  1 -> FacingRight
  (-1) -> FacingLeft
  _ -> current
