{- | AST del DSL de comportamiento de enemigos (Free monad).

Instrucciones cinemáticas y de sensado puro: velocidad, espera, ramas por
distancia y movimiento hacia jugador o spawn. Un behaviour step por frame vive
en @Domain.Logic.RunBehaviour@; @Domain.Logic.Step.advanceFrame@ lo compone con
la física y @UseCases.UpdateGame.updateGame@ lo eleva a 'GameM'.
-}
module Domain.Model.EntityBehaviour (
  -- * AST
  EntityAction (..),
  BehaviourProgram (BehaviourProgram),

  -- * Constructores
  setVelocity,
  waitFrames,
  waitThen,
  idleProgram,
  ifPlayerWithinRange,
  ifNearSpawn,
  moveTowardPlayer,
  moveTowardSpawn,
  facePlayer,
  (>>>),

  -- * Observación (tests / depuración)
  waitFramesRemaining,
)
where

import Control.Monad.Free (Free (..))
import GHC.Generics (Generic)

import Domain.ValueObjects.Velocity (Velocity)

-- | Instrucciones del DSL (functor para 'Free').
data EntityAction next
  = -- | Fija la velocidad del enemigo hasta la siguiente instrucción.
    SetVelocity Velocity next
  | -- | Mantiene la velocidad actual durante @n@ frames (un frame por behaviour step).
    WaitFrames Int next
  | -- | Continúa con @thenBranch@ o @elseBranch@ según distancia horizontal al jugador.
    IfPlayerWithinRange Float BehaviourProgram BehaviourProgram next
  | -- | Continúa según proximidad horizontal al spawn anchor del enemigo.
    IfNearSpawn Float BehaviourProgram BehaviourProgram next
  | -- | Velocidad horizontal hacia el jugador a @speed@ px/s (un behaviour step).
    MoveTowardPlayer Float next
  | -- | Velocidad horizontal hacia el spawn anchor a @speed@ px/s.
    MoveTowardSpawn Float next
  | -- | Orienta al enemigo hacia el jugador y fija velocidad cero.
    FacePlayer next
  deriving (Functor, Show, Generic)

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
    Free (IfPlayerWithinRange {}) -> "BehaviourProgram <ifPlayerWithinRange …>"
    Free (IfNearSpawn {}) -> "BehaviourProgram <ifNearSpawn …>"
    Free (MoveTowardPlayer _ _) -> "BehaviourProgram <moveTowardPlayer …>"
    Free (MoveTowardSpawn _ _) -> "BehaviourProgram <moveTowardSpawn …>"
    Free (FacePlayer _) -> "BehaviourProgram <facePlayer>"

-- | Encadena dos programas (monad @Free EntityAction@ con resultado @()@).
infixl 1 >>>

(>>>) :: BehaviourProgram -> BehaviourProgram -> BehaviourProgram
BehaviourProgram m >>> BehaviourProgram n = BehaviourProgram (m >> n)

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
  | n > 0 = waitFrames n >>> prog
  | otherwise = prog

-- | Rama por distancia horizontal al jugador (un behaviour step de decisión).
ifPlayerWithinRange ::
  Float ->
  BehaviourProgram ->
  BehaviourProgram ->
  BehaviourProgram
ifPlayerWithinRange range thenBranch elseBranch =
  BehaviourProgram (Free (IfPlayerWithinRange range thenBranch elseBranch (Pure ())))

-- | Rama por proximidad horizontal al spawn anchor.
ifNearSpawn :: Float -> BehaviourProgram -> BehaviourProgram -> BehaviourProgram
ifNearSpawn radius thenBranch elseBranch =
  BehaviourProgram (Free (IfNearSpawn radius thenBranch elseBranch (Pure ())))

-- | Un behaviour step de persecución horizontal hacia el jugador.
moveTowardPlayer :: Float -> BehaviourProgram
moveTowardPlayer speed =
  BehaviourProgram (Free (MoveTowardPlayer speed (Pure ())))

-- | Un behaviour step de retorno horizontal hacia el spawn anchor.
moveTowardSpawn :: Float -> BehaviourProgram
moveTowardSpawn speed =
  BehaviourProgram (Free (MoveTowardSpawn speed (Pure ())))

-- | Un behaviour step: mirar al jugador sin moverse.
facePlayer :: BehaviourProgram
facePlayer = BehaviourProgram (Free (FacePlayer (Pure ())))

-- | Contador de espera en la instrucción activa, si aplica.
waitFramesRemaining :: BehaviourProgram -> Maybe Int
waitFramesRemaining (BehaviourProgram prog) = case prog of
  Free (WaitFrames n _) -> Just n
  _ -> Nothing
