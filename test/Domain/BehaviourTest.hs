module Domain.BehaviourTest where

import Domain.Fixtures (dtFrame, testParams)
import Domain.Logic.EntityBehaviours (patrolHorizontal)
import Domain.Logic.RunBehaviour (runBehaviourStep, stepEnemyBehaviour)
import Domain.Logic.Step (step)
import Domain.Model.Enemy (Enemy (..), enemyPos, enemyProgram, enemyVel, mkEnemy)
import Domain.Model.EntityBehaviour (
  idleProgram,
  setVelocity,
  waitFrames,
  waitFramesRemaining,
  waitThen,
 )
import Domain.Model.Player (spawnPlayer)
import Domain.Model.World (World (..), demoWorld)
import Domain.ValueObjects.DeltaTime (deltaTime)
import Domain.ValueObjects.Input (noInput)
import Domain.ValueObjects.Position (posX, position)
import Domain.ValueObjects.Velocity (velX, velocity)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))
import UseCases.GameMonad (defaultConfig, runGameM)
import UseCases.UpdateGame (updateGame)

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
          { worldPlayer = spawnPlayer (position 0 0)
          , worldEnemies = [e0]
          , worldPlatforms = []
          }
      w1 = runBehaviourStep w0
      w2 = step testParams dtFrame noInput w1
      x1 = posX (enemyPos (head (worldEnemies w2)))
   in (x1 < 0) @?= True

unit_updateGameDtZeroSkipsBehaviour :: Assertion
unit_updateGameDtZeroSkipsBehaviour =
  case runGameM defaultConfig worldWithWait (updateGame (deltaTime 0) noInput) of
    Left err -> assertFailure (show err)
    Right ((), w') -> w' @?= worldWithWait
 where
  worldWithWait =
    World
      { worldPlayer = spawnPlayer (position 0 0)
      , worldEnemies = [mkEnemy 1 (position 50 8) (waitFrames 5)]
      , worldPlatforms = []
      }

unit_updateGamePatrolReversesVelocity :: Assertion
unit_updateGamePatrolReversesVelocity =
  let patrol = patrolHorizontal 40 2
      w0 =
        World
          { worldPlayer = spawnPlayer (position 0 0)
          , worldEnemies = [mkEnemy 1 (position 50 8) patrol]
          , worldPlatforms = []
          }
      wLeft = runTicks 1 w0
      vxLeft = velX (enemyVel (head (worldEnemies wLeft)))
      -- Tras setVel izquierda: 2 frames de wait + 1 frame que arma setVel derecha + 1 que la ejecuta
      wRight = runTicks 4 wLeft
      vxRight = velX (enemyVel (head (worldEnemies wRight)))
   in do
        assertBool "patrol starts moving left" (vxLeft < 0)
        assertBool "patrol reverses to move right" (vxRight > 0)

unit_stepWithoutInterpreterLeavesEnemyStill :: Assertion
unit_stepWithoutInterpreterLeavesEnemyStill =
  let e = mkEnemy 1 (position 50 8) (patrolHorizontal 40 90)
      w0 =
        World
          { worldPlayer = spawnPlayer (position 0 0)
          , worldEnemies = [e]
          , worldPlatforms = []
          }
      w1 = step testParams dtFrame noInput w0
   in posX (enemyPos (head (worldEnemies w1))) @?= 50

unit_updateGameAdvancesPatrolPosition :: Assertion
unit_updateGameAdvancesPatrolPosition =
  case runGameM defaultConfig demoWorld (updateGame dtFrame noInput) of
    Left err -> assertFailure (show err)
    Right ((), w') ->
      posX (enemyPos (head (worldEnemies w'))) < 50 @?= True

runTicks :: Int -> World -> World
runTicks 0 w = w
runTicks n w =
  case runGameM defaultConfig w (updateGame dtFrame noInput) of
    Left err -> error (show err)
    Right ((), w') -> runTicks (n - 1) w'
