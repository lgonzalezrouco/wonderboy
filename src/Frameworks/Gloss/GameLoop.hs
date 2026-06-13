-- | Bucle de juego Gloss: orquesta adaptadores y 'updateGame' (M8).
module Frameworks.Gloss.GameLoop (
  runGame,
)
where

import Adapters.Gloss.Config (backgroundColor, windowHeight, windowWidth)
import Adapters.Gloss.Input (KeyState, buildInput, handleKeyEvent, noKeys)
import Adapters.Gloss.Rendering (renderFrame)
import Adapters.Gloss.Time (capDeltaTime)
import Domain.DemoLevels (demoWorld)
import Domain.Logic.Health (applyDamage, resolveLifeLoss)
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.World (World (..))
import Graphics.Gloss (Display (InWindow), Picture)
import Graphics.Gloss.Interface.IO.Game (
  Event (..),
  Key (..),
  SpecialKey (KeyEsc),
  playIO,
 )
import Graphics.Gloss.Interface.IO.Game qualified as Gloss (KeyState (Down))
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import UseCases.GameMonad (defaultConfig, runGameM)
import UseCases.UpdateGame (updateGame)

-- | Estado de la aplicación Gloss (no es estado de dominio).
data AppState = AppState
  { appWorld :: World
  , appKeysHeld :: KeyState
  , appJumpPrev :: Bool
  }

-- | Estado inicial: demo con teclas sueltas y sin salto previo.
initialAppState :: AppState
initialAppState =
  AppState
    { appWorld = demoWorld
    , appKeysHeld = noKeys
    , appJumpPrev = False
    }

-- | Arranca la ventana Gloss y el bucle de juego.
runGame :: IO ()
runGame =
  playIO
    (InWindow "Wonder Boy" (windowWidth, windowHeight) (100, 100))
    backgroundColor
    60
    initialAppState
    drawFrame
    handleEvent
    advanceFrame

drawFrame :: AppState -> IO Picture
drawFrame state = pure (renderFrame (appWorld state))

handleEvent :: Event -> AppState -> IO AppState
handleEvent (EventKey (SpecialKey KeyEsc) Gloss.Down _ _) _ = exitSuccess
-- | Debug M9: aplica 1 de daño por pulsación hasta M10 aporte contacto real.
handleEvent (EventKey (Char 'h') Gloss.Down _ _) state
  | worldPhase (appWorld state) == Playing =
      pure
        state
          { appWorld =
              resolveLifeLoss (applyDamage 1 (appWorld state))
          }
handleEvent event state =
  pure state{appKeysHeld = handleKeyEvent event (appKeysHeld state)}

advanceFrame :: Float -> AppState -> IO AppState
advanceFrame _dt state
  | worldPhase (appWorld state) == GameOver = pure state
advanceFrame dt state = do
  let dt' = capDeltaTime dt
      (input, jumpPrev) = buildInput (appKeysHeld state) (appJumpPrev state)
  case runGameM defaultConfig (appWorld state) (updateGame dt' input) of
    Left err -> do
      hPutStrLn stderr ("Error: " ++ show err)
      exitFailure
    Right (_, w') ->
      pure
        state
          { appWorld = w'
          , appJumpPrev = jumpPrev
          }
