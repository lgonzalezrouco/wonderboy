{- | Intérpretes del DSL de comportamiento de enemigos.

Núcleo puro ('runBehaviourStep') y wrapper monádico ('interpretBehaviourStepM').
Un behaviour step por frame cuando @dt > 0@ (una instrucción activa); luego
@Domain.Logic.Step.step@ integra cinemática.
-}
module UseCases.InterpretBehaviour (
  -- * Intérpretes
  runBehaviourStep,
  interpretBehaviourStepM,
)
where

import Control.Monad.Free (Free (..))
import Control.Monad.State (modify)

import Domain.Logic.EntityBehaviour (
  BehaviourProgram (..),
  EntityAction (..),
 )
import Domain.Model.Enemy (Enemy (..))
import Domain.Model.World (World (..))
import Domain.ValueObjects.DeltaTime (DeltaTime, seconds)
import UseCases.GameMonad (GameM)

-- | Avanza un behaviour step en todos los enemigos del mundo (puro).
runBehaviourStep :: World -> World
runBehaviourStep w =
  w{worldEnemies = stepEnemyBehaviour <$> worldEnemies w}

{- | Wrapper monádico: delega en 'runBehaviourStep'.

Sólo corre si @dt > 0@ para alinear con identidad temporal de 'step'.
-}
interpretBehaviourStepM :: DeltaTime -> GameM ()
interpretBehaviourStepM dt
  | seconds dt == 0 = pure ()
  | otherwise = modify runBehaviourStep

-- | Un behaviour step para un enemigo.
stepEnemyBehaviour :: Enemy -> Enemy
stepEnemyBehaviour e =
  let (prog', e') = stepProgram (enemyProgram e) e
   in e'{enemyProgram = prog'}

stepProgram :: BehaviourProgram -> Enemy -> (BehaviourProgram, Enemy)
stepProgram (BehaviourProgram prog) e =
  case prog of
    Pure () -> (BehaviourProgram (Pure ()), e)
    Free (SetVelocity vel next) ->
      ( BehaviourProgram next
      , e{enemyVel = vel}
      )
    Free (WaitFrames n next)
      | n > 1 ->
          ( BehaviourProgram (Free (WaitFrames (n - 1) next))
          , e
          )
      | otherwise ->
          -- @n == 1@: último frame de espera; arma @next@ sin ejecutarlo aún.
          (BehaviourProgram next, e)
