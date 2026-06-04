-- | Punto de entrada del ejecutable. Demo de física (M3) y patrulla enemiga (M6).
module Main where

import Domain.Model.Enemy (enemyPos, enemyVel)
import Domain.Model.Player (playerOnGround, playerPos, playerVel)
import Domain.Model.World (World (..), demoWorld)
import Domain.ValueObjects.DeltaTime (deltaTime)
import Domain.ValueObjects.Input (Input (..), noInput)
import Domain.ValueObjects.PhysicsParams (PhysicsParams (..))
import Domain.ValueObjects.Position (posX)
import Domain.ValueObjects.Velocity (velY)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import UseCases.GameMonad (defaultConfig, physicsParamsFromConfig, runGameM)
import UseCases.UpdateGame (updateGame)

stepWorld :: Float -> Input -> World -> IO World
stepWorld dtSec input w =
  case runGameM defaultConfig w (updateGame (deltaTime dtSec) input) of
    Left err -> do
      hPutStrLn stderr ("Error: " ++ show err)
      exitFailure
    Right ((), w') -> pure w'

printPlayer :: String -> World -> IO ()
printPlayer label w = do
  let p = worldPlayer w
  putStrLn label
  putStrLn ("  pos:       " ++ show (playerPos p))
  putStrLn ("  vel:       " ++ show (playerVel p))
  putStrLn ("  onGround:  " ++ show (playerOnGround p))

printEnemy :: String -> World -> IO ()
printEnemy label w =
  case worldEnemies w of
    [] -> putStrLn (label ++ " (no enemies)")
    e : _ -> do
      putStrLn label
      putStrLn ("  pos: " ++ show (enemyPos e))
      putStrLn ("  vel: " ++ show (enemyVel e))

main :: IO ()
main = do
  putStrLn "=== Wonder Boy - Demo (player M3 + enemy patrol M6) ==="
  putStrLn ""

  let dt = 0.016
      w0 = demoWorld
      pp = physicsParamsFromConfig defaultConfig

  putStrLn "Tick 0 (spawn above ground):"
  printPlayer "" w0
  printEnemy "" w0
  putStrLn ""

  (wFall, n) <- fallUntilGround dt w0 120
  putStrLn ("After " ++ show n ++ " fall tick(s):")
  printPlayer "" wFall
  putStrLn ""

  let jumpInput = noInput{inputJump = True}
  wJump <- stepWorld dt jumpInput wFall
  putStrLn "One tick with jump (while grounded):"
  printPlayer "" wJump
  putStrLn $
    "  vy after jump (expect "
      ++ show (ppJumpSpeed pp)
      ++ "): "
      ++ show (velY (playerVel (worldPlayer wJump)))
  putStrLn ""

  wPatrol <- runPatrolTicks 30 dt wFall
  putStrLn "After 30 ticks (enemy patrol + player idle):"
  printEnemy "" wPatrol
  let ex = posX (enemyPos (head (worldEnemies wPatrol)))
  putStrLn ("  enemy x moved left from 50? " ++ show (ex < 50))
  putStrLn ""

  wIdle <- stepWorld 0 noInput w0
  putStrLn "Tick with dt=0 and noInput (from spawn):"
  printPlayer "" wIdle
  printEnemy "" wIdle
  putStrLn ""
  putStrLn $
    "dt=0 + noInput leaves world unchanged? "
      ++ show (wIdle == w0)

runPatrolTicks :: Int -> Float -> World -> IO World
runPatrolTicks 0 _ w = pure w
runPatrolTicks n dtSec w = do
  w' <- stepWorld dtSec noInput w
  runPatrolTicks (n - 1) dtSec w'

fallUntilGround :: Float -> World -> Int -> IO (World, Int)
fallUntilGround dtSec w0 maxTicks = loop w0 0
 where
  loop w n
    | playerOnGround (worldPlayer w) = pure (w, n)
    | n >= maxTicks = pure (w, n)
    | otherwise = do
        w' <- stepWorld dtSec noInput w
        loop w' (n + 1)
