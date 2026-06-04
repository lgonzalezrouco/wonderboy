{- | Punto de entrada del ejecutable. Demo de Milestone 3.

Simula caída con gravedad, aterrizaje en plataforma, salto y propiedad @dt=0@.
-}
module Main where

import Domain.Model.Player (playerOnGround, playerPos, playerVel)
import Domain.Model.World (World (..), initialWorld)
import Domain.ValueObjects.DeltaTime (deltaTime)
import Domain.ValueObjects.Input (Input (..), noInput)
import Domain.ValueObjects.PhysicsParams (PhysicsParams (..))
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

main :: IO ()
main = do
  putStrLn "=== Wonder Boy - Demo Milestone 3 ==="
  putStrLn ""

  let dt = 0.016
      w0 = initialWorld
      pp = physicsParamsFromConfig defaultConfig

  putStrLn "Tick 0 (spawn above ground):"
  printPlayer "" w0
  putStrLn ""

  -- Caída libre hasta estar en el suelo (máx. 120 ticks ≈ 2 s).
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

  wIdle <- stepWorld 0 noInput w0
  putStrLn "Tick with dt=0 and noInput (from spawn):"
  printPlayer "" wIdle
  putStrLn ""
  putStrLn $
    "dt=0 + noInput leaves world unchanged? "
      ++ show (wIdle == w0)

fallUntilGround :: Float -> World -> Int -> IO (World, Int)
fallUntilGround dtSec w0 maxTicks = loop w0 0
 where
  loop w n
    | playerOnGround (worldPlayer w) = pure (w, n)
    | n >= maxTicks = pure (w, n)
    | otherwise = do
        w' <- stepWorld dtSec noInput w
        loop w' (n + 1)
