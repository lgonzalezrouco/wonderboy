{- | Programas de comportamiento compuestos (catálogo DSL).

Construidos con los primitivos de @Domain.Model.EntityBehaviour@.
-}
module Domain.Logic.EntityBehaviours (
  patrolHorizontal,
)
where

import Data.Function (fix)

import Domain.Model.EntityBehaviour (
  BehaviourProgram,
  idleProgram,
  setVelocity,
  waitFrames,
  (>>>),
 )
import Domain.ValueObjects.Velocity (velocity)

{- | Patrulla horizontal indefinidamente: velocidad @±speed@ durante @frames@ frames
  por tramo (sobre suelo plano, cinemática M6). Requiere @speed > 0@ y @frames > 0@.
-}
patrolHorizontal :: Float -> Int -> BehaviourProgram
patrolHorizontal speed frames
  | speed > 0 && frames > 0 =
      fix $ \p ->
        setVelocity (velocity (-speed) 0)
          >>> waitFrames frames
          >>> setVelocity (velocity speed 0)
          >>> waitFrames frames
          >>> p
  | otherwise = idleProgram
