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
  horizontalSign,
  nearSpawnHorizontally,
  playerHorizontalDelta,
  playerHorizontalDistance,
  spawnHorizontalDelta,
 )
import Domain.Model.Enemy (Enemy (..))
import Domain.Model.EntityBehaviour (
  BehaviourProgram (..),
  EntityAction (..),
 )
import Domain.Model.World (World (..))
import Domain.ValueObjects.Facing (facingTowardHorizontal)
import Domain.ValueObjects.Frames (frameCount, tickFrames)
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
      | frameCount n > 1 ->
          ( BehaviourProgram (Free (WaitFrames (tickFrames n) next))
          , e
          )
      | otherwise ->
          (BehaviourProgram next, e)
    Free (IfPlayerWithinRange range thenBranch elseBranch _) ->
      stepBranch (playerHorizontalDistance w e <= range) thenBranch elseBranch e
    Free (IfNearSpawn radius thenBranch elseBranch _) ->
      stepBranch (nearSpawnHorizontally radius e) thenBranch elseBranch e
    Free (MoveTowardPlayer speed next) ->
      (BehaviourProgram next, moveHorizontallyToward (playerHorizontalDelta w e) speed e)
    Free (MoveTowardSpawn speed next) ->
      (BehaviourProgram next, moveHorizontallyToward (spawnHorizontalDelta e) speed e)
    Free (FacePlayer next) ->
      ( BehaviourProgram next
      , e
          { enemyVel = velocity 0 0
          , enemyFacing = facingTowardHorizontal (enemyFacing e) (playerHorizontalDelta w e)
          }
      )

{- | Un behaviour step de decisión: elige rama y deja la velocidad en cero.

Las ramas son la cola del programa (los constructores 'IfPlayerWithinRange' /
'IfNearSpawn' fijan su continuación a 'Pure ()'), así que no hay un @next@ que
encadenar tras la rama elegida.
-}
stepBranch ::
  Bool ->
  BehaviourProgram ->
  BehaviourProgram ->
  Enemy ->
  (BehaviourProgram, Enemy)
stepBranch cond thenBranch elseBranch e =
  (if cond then thenBranch else elseBranch, e{enemyVel = velocity 0 0})

moveHorizontallyToward :: Float -> Float -> Enemy -> Enemy
moveHorizontallyToward dx speed e =
  let dir = horizontalSign dx
   in e
        { enemyVel = velocity (dir * speed) 0
        , enemyFacing = facingTowardHorizontal (enemyFacing e) dx
        }
