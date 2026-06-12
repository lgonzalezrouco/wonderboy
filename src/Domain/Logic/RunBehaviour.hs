{- | Intérprete puro del DSL de comportamiento de enemigos.

Un behaviour step por frame cuando @dt > 0@ en el ciclo de update; luego
@Domain.Logic.Step.step@ integra cinemática.
-}
module Domain.Logic.RunBehaviour (
  runBehaviourStep,
  stepEnemyBehaviour,
)
where

import Control.Monad.Free (Free (..))

import Domain.Model.Enemy (Enemy (..))
import Domain.Model.EntityBehaviour (
  BehaviourProgram (..),
  EntityAction (..),
 )
import Domain.Model.World (World (..))

-- | Avanza un behaviour step en todos los enemigos del mundo (puro).
runBehaviourStep :: World -> World
runBehaviourStep w =
  w{worldEnemies = stepEnemyBehaviour <$> worldEnemies w}

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
