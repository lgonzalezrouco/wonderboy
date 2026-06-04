{- | Orquestación de 'updateGame' en la costura de 'UseCases': orden de fases,
política de frame congelado (@dt = 0@) y bucle multi-frame ('runFrames').
-}
module UseCases.UpdateGameTest where

import Domain.DemoLevels (demoWorld)
import Domain.Fixtures (dtFrame)
import Domain.Logic.EntityBehaviours (patrolHorizontal)
import Domain.Model.Enemy (enemyPos, enemyVel, mkEnemy)
import Domain.Model.EntityBehaviour (waitFrames)
import Domain.Model.Player (spawnPlayer)
import Domain.Model.World (World (..))
import Domain.ValueObjects.DeltaTime (deltaTime)
import Domain.ValueObjects.Input (noInput)
import Domain.ValueObjects.Position (posX, position)
import Domain.ValueObjects.Velocity (velX)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))
import UseCases.GameMonad (defaultConfig, runGameM)
import UseCases.UpdateGame (runFrames, updateGame)

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

unit_updateGameAdvancesPatrolPosition :: Assertion
unit_updateGameAdvancesPatrolPosition =
  case runGameM defaultConfig demoWorld (updateGame dtFrame noInput) of
    Left err -> assertFailure (show err)
    Right ((), w') ->
      posX (enemyPos (head (worldEnemies w'))) < 50 @?= True

-- | Corre @n@ frames sobre el harness compartido, abortando si hubiera un error.
runTicks :: Int -> World -> World
runTicks n = either (error . show) id . runFrames defaultConfig n dtFrame noInput
