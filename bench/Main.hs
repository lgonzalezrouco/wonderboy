module Main (main) where

import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Domain.Logic.BehaviourCatalog (defaultProgramForKind)
import Domain.Logic.Frame (
  FrameParams (..),
  FrameResult (..),
  PlayingFrame (..),
  advanceSimulationFrame,
 )
import Domain.Model.Enemy (Enemy (..), spawnEnemy)
import Domain.Model.EnemyKind (EnemyKind (SnailKind))
import Domain.Model.Player (Player (..))
import Domain.Model.Projectile (Projectile (..))
import Domain.Model.World (World (..), initialWorld)
import Domain.ValueObjects.DeltaTime (DeltaTime, deltaTime)
import Domain.ValueObjects.Health (healthPoints)
import Domain.ValueObjects.Input (noInput)
import Domain.ValueObjects.Lives (lives)
import Domain.ValueObjects.Position (posX, posY, position)
import Domain.ValueObjects.Score (score)
import Domain.ValueObjects.Velocity (velX, velY)
import UseCases.GameMonad (
  GameConfig (..),
  combatParamsFromConfig,
  defaultConfig,
  lifeParamsFromConfig,
  physicsParamsFromConfig,
  throwParamsFromConfig,
 )

-- | Timestep fijo para el benchmark: 0.016 s (~16 ms), es decir, un frame a 60 Hz.
dtFrame :: DeltaTime
dtFrame = deltaTime 0.016

frameParams :: FrameParams
frameParams =
  FrameParams
    { fpPhysics = physicsParamsFromConfig defaultConfig
    , fpLife = lifeParamsFromConfig defaultConfig
    , fpCombat = combatParamsFromConfig defaultConfig
    , fpThrow = throwParamsFromConfig defaultConfig
    }

benchWorld :: World
benchWorld =
  initialWorld
    { worldEnemies = []
    }
_unusedEnemyCtors :: (EnemyKind, Int -> World)
_unusedEnemyCtors = (SnailKind, \_ -> initialWorld{worldEnemies = [spawnEnemy 1 SnailKind (position 600 8) (defaultProgramForKind SnailKind)]})

benchFrame0 :: PlayingFrame
benchFrame0 =
  PlayingFrame
    { pfWorld = benchWorld
    , pfLives = lives 3
    , pfScore = score 0
    , pfLevelIndex = 1
    }

oneFrame :: PlayingFrame -> PlayingFrame
oneFrame pf =
  let r = advanceSimulationFrame frameParams (gcLevelCount defaultConfig) dtFrame noInput pf
   in pf{pfWorld = frWorld r, pfLives = frLives r, pfScore = frScore r}

worldChecksum :: World -> Double
worldChecksum w =
  playerC (worldPlayer w)
    + sum (map enemyC (worldEnemies w))
    + sum (map projC (worldProjectiles w))
    + fromIntegral
      ( length (worldEnemies w)
          + length (worldProjectiles w)
          + length (worldMovingPlatforms w)
          + length (worldCrumblingPlatforms w)
          + length (worldFallingHazards w)
      )
 where
  f = realToFrac :: Float -> Double
  playerC p =
    f (posX (playerPos p))
      + f (posY (playerPos p))
      + f (velX (playerVel p))
      + f (velY (playerVel p))
      + fromIntegral (healthPoints (playerHealth p))
  enemyC e =
    f (posX (enemyPos e))
      + f (posY (enemyPos e))
      + f (velX (enemyVel e))
      + f (velY (enemyVel e))
      + fromIntegral (healthPoints (enemyHealth e))
  projC pr = f (posX (projectilePos pr)) + f (posY (projectilePos pr))

forceWorld :: World -> Bool
forceWorld w = w == w

runStrict :: Int -> Double
runStrict n = go n benchFrame0 0
 where
  go k pf !acc
    | k <= 0 = acc
    | otherwise =
        let pf' = oneFrame pf
            w' = pfWorld pf'
            c = worldChecksum w'
         in forceWorld w' `seq` c `seq` go (k - 1) pf' (acc + c)

runLazy :: Int -> Double
runLazy n = go n benchFrame0 0
 where
  go k pf acc
    | k <= 0 = acc
    | otherwise =
        let pf' = oneFrame pf
         in go (k - 1) pf' (acc + worldChecksum (pfWorld pf'))

main :: IO ()
main = do
  args <- getArgs
  let (modeStr, n) = case args of
        (m : k : _) -> (m, fromMaybe defaultFrames (readMaybe k))
        [m] -> (m, defaultFrames)
        [] -> ("strict", defaultFrames)
      total = case modeStr of
        "lazy" -> runLazy n
        _ -> runStrict n
  putStrLn (modeStr ++ " frames=" ++ show n ++ " checksum=" ++ show total)
 where
  defaultFrames = 200000
