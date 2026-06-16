{- | Intérprete puro del DSL de comportamiento de enemigos.

Un behaviour step por frame cuando @dt > 0@ en el ciclo de update; luego
@Domain.Logic.Step.step@ integra cinemática. M13: el intérprete lee 'World' para
sensado y ramas deterministas.
-}
module Domain.Logic.RunBehaviour (
  runBehaviourStep,
  stepEnemyBehaviour,
)
where

import Control.Monad.Free (Free (..))

import Domain.Logic.BehaviourSensing (
  facingTowardHorizontal,
  horizontalSign,
  nearSpawnHorizontally,
  playerHorizontalDistance,
 )
import Domain.Model.Enemy (Enemy (..))
import Domain.Model.EntityBehaviour (
  BehaviourProgram (..),
  EntityAction (..),
  (>>>),
 )
import Domain.Model.Player (playerPos)
import Domain.Model.World (World (..))
import Domain.ValueObjects.Position (posX)
import Domain.ValueObjects.Velocity (velocity)

-- | Avanza un behaviour step en todos los enemigos del mundo (puro).
runBehaviourStep :: World -> World
runBehaviourStep w =
  w{worldEnemies = map (stepEnemyBehaviour w) (worldEnemies w)}

-- | Un behaviour step para un enemigo.
stepEnemyBehaviour :: World -> Enemy -> Enemy
stepEnemyBehaviour w e =
  let (prog', e') = stepProgram w (enemyProgram e) e
   in e'{enemyProgram = prog'}

stepProgram :: World -> BehaviourProgram -> Enemy -> (BehaviourProgram, Enemy)
stepProgram w (BehaviourProgram prog) e =
  case prog of
    Pure () -> (BehaviourProgram (Pure ()), e)
    Free (SetVelocity vel next) ->
      (BehaviourProgram next, e{enemyVel = vel})
    Free (WaitFrames n next)
      | n > 1 ->
          ( BehaviourProgram (Free (WaitFrames (n - 1) next))
          , e
          )
      | otherwise ->
          (BehaviourProgram next, e)
    Free (IfPlayerWithinRange range thenBranch elseBranch next) ->
      let chosen =
            if playerHorizontalDistance w e <= range
              then thenBranch
              else elseBranch
       in (chosen >>> BehaviourProgram next, e)
    Free (IfNearSpawn radius thenBranch elseBranch next) ->
      let chosen =
            if nearSpawnHorizontally radius e
              then thenBranch
              else elseBranch
       in (chosen >>> BehaviourProgram next, e)
    Free (MoveTowardPlayer speed next) ->
      let dx = posX (playerPos (worldPlayer w)) - posX (enemyPos e)
          dir = horizontalSign dx
          vel = velocity (dir * speed) 0
          facing = facingTowardHorizontal (enemyFacing e) dx
       in (BehaviourProgram next, e{enemyVel = vel, enemyFacing = facing})
    Free (MoveTowardSpawn speed next) ->
      let dx = posX (enemySpawnPos e) - posX (enemyPos e)
          dir = horizontalSign dx
          vel = velocity (dir * speed) 0
          facing = facingTowardHorizontal (enemyFacing e) dx
       in (BehaviourProgram next, e{enemyVel = vel, enemyFacing = facing})
    Free (FacePlayer next) ->
      let dx = posX (playerPos (worldPlayer w)) - posX (enemyPos e)
          facing = facingTowardHorizontal (enemyFacing e) dx
       in (BehaviourProgram next, e{enemyVel = velocity 0 0, enemyFacing = facing})
