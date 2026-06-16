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
import UseCases.GameMonad (GameState, defaultConfig, gameViewFromState, initialGameState, runGameM)
import UseCases.UpdateGame (updateGame)

-- | Estado de la aplicación Gloss (no es estado de dominio).
data AppState = AppState
  { appGameState :: GameState
  , appKeysHeld :: KeyState
  , appJumpPrev :: Bool
  , appAttackPrev :: Bool
  }

-- | Estado inicial: demo con teclas sueltas y sin salto previo.
initialAppState :: AppState
initialAppState =
  AppState
    { appGameState = initialGameState defaultConfig demoWorld
    , appKeysHeld = noKeys
    , appJumpPrev = False
    , appAttackPrev = False
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
drawFrame state = pure (renderFrame (gameViewFromState (appGameState state)))

handleEvent :: Event -> AppState -> IO AppState
handleEvent (EventKey (SpecialKey KeyEsc) Gloss.Down _ _) _ = exitSuccess
handleEvent event state =
  pure state{appKeysHeld = handleKeyEvent event (appKeysHeld state)}

advanceFrame :: Float -> AppState -> IO AppState
advanceFrame dt state = do
  let dt' = capDeltaTime dt
      (input, jumpPrev, attackPrev) =
        buildInput (appKeysHeld state) (appJumpPrev state) (appAttackPrev state)
  case runGameM defaultConfig (appGameState state) (updateGame dt' input) of
    Left err -> do
      hPutStrLn stderr ("Error: " ++ show err)
      exitFailure
    Right (_, gs') ->
      pure
        state
          { appGameState = gs'
          , appJumpPrev = jumpPrev
          , appAttackPrev = attackPrev
          }
