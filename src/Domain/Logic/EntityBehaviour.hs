{- | AST del DSL de comportamiento de enemigos (Free monad).

Instrucciones cinemáticas puras: 'setVelocity' y 'waitFrames'. La ejecución
(un behaviour step por frame) vive en @UseCases.InterpretBehaviour@.
-}
module Domain.Logic.EntityBehaviour (
  -- * AST
  EntityAction (..),
  BehaviourProgram (..),

  -- * Constructores
  setVelocity,
  waitFrames,
  waitThen,
  patrolHorizontal,
  idleProgram,

  -- * Observación (tests / depuración)
  waitFramesRemaining,
)
where

import Control.Monad.Free (Free (..))
import Data.Function (fix)
import GHC.Generics (Generic)

import Domain.ValueObjects.Velocity (Velocity, velocity)

-- | Instrucciones del DSL (functor para 'Free').
data EntityAction next
  = -- | Fija la velocidad del enemigo hasta la siguiente instrucción.
    SetVelocity Velocity next
  | -- | Mantiene la velocidad actual durante @n@ frames (un frame por behaviour step).
    WaitFrames Int next
  deriving (Functor, Eq, Show, Generic)

{- | Programa de comportamiento de un enemigo.

Envuelve @Free EntityAction ()@ para no exponer el functor en el modelo.
-}
newtype BehaviourProgram = BehaviourProgram
  {unBehaviourProgram :: Free EntityAction ()}
  deriving (Generic)

-- Nota: 'BehaviourProgram' no tiene instancia 'Eq' a propósito. Un 'Free
-- EntityAction ()' es una descripción posiblemente cíclica (p. ej. la patrulla
-- de 'patrolHorizontal' construida con @fix@), así que no admite una igualdad
-- estructural total ni barata. Una comparación parcial (solo la instrucción
-- activa) rompería las leyes de 'Eq' —programas distintos compararían iguales—,
-- por eso se observa el programa con funciones explícitas como
-- 'waitFramesRemaining' en lugar de '=='.

instance Show BehaviourProgram where
  show (BehaviourProgram prog) = case prog of
    Pure () -> "BehaviourProgram <done>"
    Free (SetVelocity _ _) -> "BehaviourProgram <setVelocity …>"
    Free (WaitFrames n _) -> "BehaviourProgram <waitFrames " ++ show n ++ ">"

-- | Programa vacío: no modifica velocidad en behaviour steps.
idleProgram :: BehaviourProgram
idleProgram = BehaviourProgram (Pure ())

-- | Fija velocidad (px/s) en el siguiente behaviour step.
setVelocity :: Velocity -> BehaviourProgram
setVelocity vel =
  BehaviourProgram (Free (SetVelocity vel (Pure ())))

-- | Espera @n@ frames sin cambiar velocidad (@n <= 0@ no espera).
waitFrames :: Int -> BehaviourProgram
waitFrames n
  | n > 0 = BehaviourProgram (Free (WaitFrames n (Pure ())))
  | otherwise = idleProgram

-- | Ejecuta @prog@ tras esperar @n@ frames (@n > 0@).
waitThen :: Int -> BehaviourProgram -> BehaviourProgram
waitThen n prog
  | n > 0 = BehaviourProgram (Free (WaitFrames n (unBehaviourProgram prog)))
  | otherwise = prog

-- | Contador de espera en la instrucción activa, si aplica.
waitFramesRemaining :: BehaviourProgram -> Maybe Int
waitFramesRemaining (BehaviourProgram prog) = case prog of
  Free (WaitFrames n _) -> Just n
  _ -> Nothing

{- | Patrulla horizontal indefinidamente: velocidad @±speed@ durante @frames@ frames
  por tramo (sobre suelo plano, cinemática M6). Requiere @speed > 0@ y @frames > 0@.
-}
patrolHorizontal :: Float -> Int -> BehaviourProgram
patrolHorizontal speed frames
  | speed > 0 && frames > 0 =
      BehaviourProgram (fix body)
  | otherwise = idleProgram
 where
  body loop = do
    setVel (-speed) 0
    wait frames
    setVel speed 0
    wait frames
    loop
  setVel vx vy = Free (SetVelocity (velocity vx vy) (Pure ()))
  wait n = Free (WaitFrames n (Pure ()))
