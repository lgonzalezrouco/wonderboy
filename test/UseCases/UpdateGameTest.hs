{- | Orquestación de 'updateGame' en la costura de 'UseCases': orden de fases,
política de frame congelado (@dt = 0@) y bucle multi-frame ('runFrames').
-}
module UseCases.UpdateGameTest where

import Domain.Fixtures (demoWorld, dtFrame, mkTestPickup, worldWithPickups)
import Domain.Logic.EntityBehaviours (patrolHorizontal)
import Domain.Model.Enemy (enemyPos, enemyVel, mkEnemy)
import Domain.Model.EntityBehaviour (waitFrames)
import Domain.Model.ExitZone (ExitZone (..), defaultExitZone)
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.Player (playerAttackFrames, spawnPlayer)
import Domain.Model.World (World (..), defaultMaxHealth, worldPickups, worldPlayer)
import Domain.ValueObjects.DeltaTime (deltaTime)
import Domain.ValueObjects.Frames (frames)
import Domain.ValueObjects.Input (noInput)
import Domain.ValueObjects.Lives (lives)
import Domain.ValueObjects.Position (posX, position)
import Domain.ValueObjects.Score (score)
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

playingState :: World -> GameState
playingState w =
  GameState
    { gsWorld = w
    , gsLives = gcStartingLives defaultConfig
    , gsPhase = Playing
    , gsScore = score 0
    , gsLevelIndex = 1
    }

gameOverState :: World -> GameState
gameOverState w =
  GameState
    { gsWorld = w
    , gsLives = lives 0
    , gsPhase = GameOver
    , gsScore = score 0
    , gsLevelIndex = 1
    }

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
      , worldEnemies = [mkEnemy 1 (position 50 8) (waitFrames (frames 5))]
      , worldPlatforms = []
      , worldMovingPlatforms = []
      , worldSpawnPoint = position 0 0
      , worldPickups = []
      , worldMinScore = score 0
      , worldExit = defaultExitZone
      , worldProjectiles = []
      , worldNextProjectileId = 1
      , worldFallingHazards = []
      , worldCrumblingPlatforms = []
      , worldBossArena = Nothing
      , worldBossArenaEngaged = False
      }

unit_updateGamePatrolReversesVelocity :: Assertion
unit_updateGamePatrolReversesVelocity =
  let patrol = patrolHorizontal 40 (frames 2)
      w0 =
        World
          { worldPlayer = spawnPlayer defaultMaxHealth (position 0 0)
          , worldEnemies = [mkEnemy 1 (position 50 8) patrol]
          , worldPlatforms = []
          , worldMovingPlatforms = []
          , worldSpawnPoint = position 0 0
          , worldPickups = []
          , worldMinScore = score 0
          , worldExit = ExitZone (position 500 0) 32 64
          , worldProjectiles = []
          , worldNextProjectileId = 1
          , worldFallingHazards = []
          , worldCrumblingPlatforms = []
          , worldBossArena = Nothing
          , worldBossArenaEngaged = False
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
        e : _ -> posX (enemyPos e) < 160 @?= True
        [] -> assertFailure "expected one enemy after one frame"

unit_gameOverSkipsUpdate :: Assertion
unit_gameOverSkipsUpdate =
  let gs0 = gameOverState demoWorld
   in case runGameM defaultConfig gs0 (updateGame dtFrame noInput) of
        Left err -> assertFailure (show err)
        Right ((), gs') -> gs' @?= gs0

unit_updateGameDtZeroSkipsCombat :: Assertion
unit_updateGameDtZeroSkipsCombat =
  let w =
        demoWorld
          { worldPlayer =
              (spawnPlayer defaultMaxHealth (position 0 80))
                { playerAttackFrames = frames 3
                }
          }
      gs0 = playingState w
   in case runGameM defaultConfig gs0 (updateGame (deltaTime 0) noInput) of
        Left err -> assertFailure (show err)
        Right ((), gs') ->
          playerAttackFrames (worldPlayer (gsWorld gs')) @?= frames 3

unit_updateGameCollectsPickup :: Assertion
unit_updateGameCollectsPickup =
  let pickup = mkTestPickup 1 (position 0 8) 100
      w = worldWithPickups (position 0 8) [pickup]
      gs0 = playingState w
   in case runGameM defaultConfig gs0 (updateGame dtFrame noInput) of
        Left err -> assertFailure (show err)
        Right ((), gs') -> do
          gsScore gs' @?= score 100
          worldPickups (gsWorld gs') @?= []

unit_gameOverSkipsPickup :: Assertion
unit_gameOverSkipsPickup =
  let pickup = mkTestPickup 1 (position 0 8) 100
      w = worldWithPickups (position 0 8) [pickup]
      gs0 = gameOverState w
   in case runGameM defaultConfig gs0 (updateGame dtFrame noInput) of
        Left err -> assertFailure (show err)
        Right ((), gs') -> do
          gsScore gs' @?= score 0
          worldPickups (gsWorld gs') @?= [pickup]

unit_updateGameLevelCompleteWhenHybridWin :: Assertion
unit_updateGameLevelCompleteWhenHybridWin =
  let exitZone =
        ExitZone
          { exitPos = position 0 0
          , exitWidth = 32
          , exitHeight = 64
          }
      w =
        demoWorld
          { worldPlayer = spawnPlayer defaultMaxHealth (position 0 0)
          , worldMinScore = score 0
          , worldExit = exitZone
          , worldEnemies = []
          }
      gs0 = playingState w
   in case runGameM defaultConfig gs0 (updateGame dtFrame noInput) of
        Left err -> assertFailure (show err)
        Right ((), gs') -> gsPhase gs' @?= LevelComplete

-- | Corre @n@ frames sobre el harness compartido, abortando si hubiera un error.
runTicks :: Int -> GameState -> GameState
runTicks n = either (error . show) id . runFrames defaultConfig n dtFrame noInput
