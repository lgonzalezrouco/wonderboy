{- | Orquestación de 'updateGame' en la costura de 'UseCases': orden de fases,
política de frame congelado (@dt = 0@) y bucle multi-frame ('runFrames').
-}
module UseCases.UpdateGameTest where

import Domain.DemoLevels (demoWorld)
import Domain.Fixtures (dtFrame)
import Domain.Logic.EntityBehaviours (patrolHorizontal)
import Domain.Model.Enemy (enemyPos, enemyVel, mkEnemy)
import Domain.Model.EntityBehaviour (waitFrames)
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.Player (playerAttackFrames, spawnPlayer)
import Domain.Model.World (World (..), defaultMaxHealth, worldPlayer)
import Domain.ValueObjects.DeltaTime (deltaTime)
import Domain.ValueObjects.Input (noInput)
import Domain.ValueObjects.Position (posX, position)
import Domain.ValueObjects.Velocity (velX)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))
import UseCases.GameMonad (
  GameState (..),
  defaultConfig,
  gcStartingLives,
  initialGameState,
  runGameM,
 )
import UseCases.UpdateGame (runFrames, updateGame)

unit_updateGameDtZeroSkipsBehaviour :: Assertion
unit_updateGameDtZeroSkipsBehaviour =
  case runGameM defaultConfig gsWithWait (updateGame (deltaTime 0) noInput) of
    Left err -> assertFailure (show err)
    Right ((), gs') -> gs' @?= gsWithWait
 where
  gsWithWait = initialGameState defaultConfig worldWithWait
  worldWithWait =
    World
      { worldPlayer = spawnPlayer defaultMaxHealth (position 0 0)
      , worldEnemies = [mkEnemy 1 (position 50 8) (waitFrames 5)]
      , worldPlatforms = []
      , worldSpawnPoint = position 0 0
      }

unit_updateGamePatrolReversesVelocity :: Assertion
unit_updateGamePatrolReversesVelocity =
  let patrol = patrolHorizontal 40 2
      w0 =
        World
          { worldPlayer = spawnPlayer defaultMaxHealth (position 0 0)
          , worldEnemies = [mkEnemy 1 (position 50 8) patrol]
          , worldPlatforms = []
          , worldSpawnPoint = position 0 0
          }
      gs0 = initialGameState defaultConfig w0
      gsLeft = runTicks 1 gs0
      gsRight = runTicks 4 gsLeft
   in case (worldEnemies (gsWorld gsLeft), worldEnemies (gsWorld gsRight)) of
        (eLeft : _, eRight : _) -> do
          assertBool "patrol starts moving left" (velX (enemyVel eLeft) < 0)
          assertBool "patrol reverses to move right" (velX (enemyVel eRight) > 0)
        _ -> assertFailure "expected one enemy in each sampled world"

unit_updateGameAdvancesPatrolPosition :: Assertion
unit_updateGameAdvancesPatrolPosition =
  case runGameM defaultConfig (initialGameState defaultConfig demoWorld) (updateGame dtFrame noInput) of
    Left err -> assertFailure (show err)
    Right ((), gs') ->
      case worldEnemies (gsWorld gs') of
        e : _ -> posX (enemyPos e) < 50 @?= True
        [] -> assertFailure "expected one enemy after one frame"

unit_gameOverSkipsUpdate :: Assertion
unit_gameOverSkipsUpdate =
  let gs0 =
        GameState
          { gsWorld = demoWorld
          , gsLives = 0
          , gsPhase = GameOver
          }
   in case runGameM defaultConfig gs0 (updateGame dtFrame noInput) of
        Left err -> assertFailure (show err)
        Right ((), gs') -> gs' @?= gs0

unit_updateGameDtZeroSkipsCombat :: Assertion
unit_updateGameDtZeroSkipsCombat =
  let w =
        demoWorld
          { worldPlayer =
              (spawnPlayer defaultMaxHealth (position 0 80))
                { playerAttackFrames = 3
                }
          }
      gs0 =
        GameState
          { gsWorld = w
          , gsLives = gcStartingLives defaultConfig
          , gsPhase = Playing
          }
   in case runGameM defaultConfig gs0 (updateGame (deltaTime 0) noInput) of
        Left err -> assertFailure (show err)
        Right ((), gs') ->
          playerAttackFrames (worldPlayer (gsWorld gs')) @?= 3

-- | Corre @n@ frames sobre el harness compartido, abortando si hubiera un error.
runTicks :: Int -> GameState -> GameState
runTicks n = either (error . show) id . runFrames defaultConfig n dtFrame noInput
