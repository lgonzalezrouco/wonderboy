{- | Punto de entrada del ejecutable. Demo de Milestone 2.

Corre 3 ticks de `updateGame` con input de movimiento hacia la derecha
y muestra el estado del mundo en cada step.
Verifica que el stack monádico funciona de punta a punta sin Gloss.
-}
module Main where

import Domain.Model.World (World (..), initialWorld)
import Domain.ValueObjects.DeltaTime (DeltaTime, deltaTime)
import Domain.ValueObjects.Input (Input (..), noInput)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import UseCases.GameMonad (defaultConfig, runGameM)
import UseCases.UpdateGame (updateGame)

{- | Ejecuta un tick de `updateGame` a partir de un 'World' dado.

Usa 'runGameM' 'defaultConfig' para correr la pila monádica.
En caso de error imprime en stderr y termina con código distinto de cero.
-}
stepWorld :: DeltaTime -> Input -> World -> IO World
stepWorld dt input w =
  case runGameM defaultConfig w (updateGame dt input) of
    Left err -> do
      hPutStrLn stderr ("Error: " ++ show err)
      exitFailure
    Right ((), w') -> pure w'

-- `runGameM :: GameConfig -> GameState -> GameM a -> Either GameError (a, GameState)`
-- Pasamos `defaultConfig` (GameConfig), `w` (World = GameState), y la acción.
-- El resultado es `Right ((), w')` donde `w'` es el mundo actualizado.

main :: IO ()
main = do
  putStrLn "=== Wonder Boy - Demo Milestone 2 ==="
  putStrLn ""

  let dt = deltaTime 0.016 -- 16 ms ≈ 60 FPS
      moveRight = noInput{inputRight = True}
  -- `noInput { inputRight = True }` usa actualización de record:
  -- copia `noInput` con `inputRight = True`; el resto queda False.

  -- Corremos 3 ticks con el jugador moviéndose a la derecha.
  let w0 = initialWorld
  w1 <- stepWorld dt moveRight w0
  w2 <- stepWorld dt moveRight w1
  w3 <- stepWorld dt moveRight w2

  putStrLn "Tick 0 (inicial):"
  print (worldPlayer w0)
  putStrLn ""

  putStrLn "Tick 1 (-> derecha, 16 ms):"
  print (worldPlayer w1)
  putStrLn ""

  putStrLn "Tick 2 (-> derecha, 16 ms):"
  print (worldPlayer w2)
  putStrLn ""

  putStrLn "Tick 3 (-> derecha, 16 ms):"
  print (worldPlayer w3)
  putStrLn ""

  -- Verificamos la propiedad precursora del test de M5:
  -- dt=0 con noInput no debe desplazar al jugador.
  wIdle <- stepWorld (deltaTime 0) noInput w0
  putStrLn "Tick con dt=0 y noInput (player debe quedar en origen):"
  print (worldPlayer wIdle)
  putStrLn ""
  putStrLn $
    "?dt=0 + noInput es identidad en posicion? "
      ++ show (worldPlayer wIdle == worldPlayer w0)

-- `let` en `do` introduce ligaduras locales. Cada línea puede usar las
-- definidas antes en el mismo bloque (evaluación lazy: sólo se calculan
-- cuando se usan).
