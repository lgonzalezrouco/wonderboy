{- | Semántica pura del DSL de comportamiento (M6): pasos de behaviour y su
composición con la física en el dominio. La orquestación en 'GameM' vive en
'UseCases.UpdateGameTest'.
-}
module Domain.BehaviourTest where

import Domain.Fixtures (dtFrame, testParams)
import Domain.Logic.EntityBehaviours (patrolHorizontal)
import Domain.Logic.RunBehaviour (runBehaviourStep, stepEnemyBehaviour)
import Domain.Logic.Step (step)
import Domain.Model.Enemy (enemyPos, enemyProgram, enemyVel, mkEnemy)
import Domain.Model.EntityBehaviour (
  idleProgram,
  setVelocity,
  waitFrames,
  waitFramesRemaining,
  waitThen,
 )
import Domain.Model.Player (spawnPlayer)
import Domain.Model.World (World (..), defaultMaxHealth)
import Domain.ValueObjects.Input (noInput)
import Domain.ValueObjects.Position (posX, position)
import Domain.ValueObjects.Velocity (velX, velocity)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))

unit_waitFramesDecrements :: Assertion
unit_waitFramesDecrements =
  let e = mkEnemy 0 (position 0 0) (waitFrames 3)
      e' = stepEnemyBehaviour e
   in waitFramesRemaining (enemyProgram e') @?= Just 2

unit_waitFramesHoldsVelocity :: Assertion
unit_waitFramesHoldsVelocity = do
  let e0 = mkEnemy 0 (position 0 0) (waitFrames 2)
      e1 = stepEnemyBehaviour e0
      e2 = stepEnemyBehaviour e1
  velX (enemyVel e0) @?= 0
  velX (enemyVel e1) @?= 0
  velX (enemyVel e2) @?= 0

unit_waitThenRunsSetVelocityAfterWait :: Assertion
unit_waitThenRunsSetVelocityAfterWait = do
  let prog = waitThen 1 (setVelocity (velocity 7 0))
      e0 = mkEnemy 0 (position 0 0) prog
      e1 = stepEnemyBehaviour e0
      e2 = stepEnemyBehaviour e1
  waitFramesRemaining (enemyProgram e1) @?= Nothing
  velX (enemyVel e1) @?= 0
  velX (enemyVel e2) @?= 7

unit_setVelocityOnStep :: Assertion
unit_setVelocityOnStep =
  let e = mkEnemy 0 (position 0 0) (setVelocity (velocity 10 0))
      e' = stepEnemyBehaviour e
   in velX (enemyVel e') @?= 10

unit_idleProgramNoOp :: Assertion
unit_idleProgramNoOp =
  let e = mkEnemy 0 (position 0 0) idleProgram
      e' = stepEnemyBehaviour e
   in e' @?= e

unit_behaviourThenStepMovesEnemy :: Assertion
unit_behaviourThenStepMovesEnemy =
  let e0 = mkEnemy 0 (position 0 8) (setVelocity (velocity (-40) 0))
      w0 =
        World
          { worldPlayer = spawnPlayer defaultMaxHealth (position 0 0)
          , worldEnemies = [e0]
          , worldPlatforms = []
          , worldSpawnPoint = position 0 0
          , worldPickups = []
          , worldMinScore = 0
          }
      w1 = runBehaviourStep w0
      w2 = step testParams dtFrame noInput w1
   in case worldEnemies w2 of
        e : _ -> (posX (enemyPos e) < 0) @?= True
        [] -> assertFailure "expected one enemy after stepping"

unit_stepDoesNotRunBehaviour :: Assertion
unit_stepDoesNotRunBehaviour =
  let e = mkEnemy 1 (position 50 8) (patrolHorizontal 40 90)
      w0 =
        World
          { worldPlayer = spawnPlayer defaultMaxHealth (position 0 0)
          , worldEnemies = [e]
          , worldPlatforms = []
          , worldSpawnPoint = position 0 0
          , worldPickups = []
          , worldMinScore = 0
          }
      w1 = step testParams dtFrame noInput w0
   in case worldEnemies w1 of
        enemy : _ -> posX (enemyPos enemy) @?= 50
        [] -> assertFailure "expected one enemy to remain"
