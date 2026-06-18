{-# LANGUAGE LambdaCase #-}

-- | Bucle de juego Gloss: orquesta adaptadores y 'updateGame' (M8).
module Frameworks.Gloss.GameLoop (
  runGame,
)
where

import Adapters.Gloss.Config (backgroundColor, windowHeight, windowWidth)
import Adapters.Gloss.Input (KeyState, buildInput, handleKeyEvent, noKeys)
import Adapters.Gloss.Rendering (renderFrame)
import Adapters.Gloss.Sprites (SpriteCatalog, loadSpriteCatalog)
import Adapters.Gloss.Time (capDeltaTime)
import Adapters.LevelFile (readLevelFile)
import Domain.Model.World (World)
import Domain.ValueObjects.DeltaTime (isFrozen)
import Graphics.Gloss (Display (InWindow), Picture)
import Graphics.Gloss.Interface.IO.Game (
  Event (..),
  Key (..),
  SpecialKey (KeyEsc, KeyF1),
  playIO,
 )
import Graphics.Gloss.Interface.IO.Game qualified as Gloss (KeyState (Down))
import Paths_wonderboy_hs (getDataFileName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import UseCases.GameMonad (GameError (..), GameState, defaultConfig, gameViewFromState, initialGameState, runGameM)
import UseCases.LoadLevel (loadLevelFromText)
import UseCases.UpdateGame (updateGame)

-- | Estado de la aplicación Gloss (no es estado de dominio).
data AppState = AppState
  { appGameState :: GameState
  , appSprites :: SpriteCatalog
  , appRenderFrame :: Int
  , appKeysHeld :: KeyState
  , appJumpPrev :: Bool
  , appAttackPrev :: Bool
  , appShowHitboxes :: Bool
  }

-- | Arranca la ventana Gloss y el bucle de juego.
runGame :: IO ()
runGame = do
  path <- getDataFileName "levels/demo.json"
  world <- loadWorld path
  sprites <- loadSpriteCatalog
  playIO
    (InWindow "Wonder Boy" (windowWidth, windowHeight) (100, 100))
    backgroundColor
    60
    (initialAppState sprites world)
    drawFrame
    handleEvent
    advanceFrame

-- | Lee y construye el mundo desde un archivo de nivel JSON.
loadWorld :: FilePath -> IO World
loadWorld path =
  readLevelFile path >>= \case
    Left err -> exitWithError err
    Right txt ->
      case loadLevelFromText txt of
        Left (GameError err) -> exitWithError err
        Right world -> pure world
 where
  exitWithError err = hPutStrLn stderr ("Error: " ++ err) >> exitFailure

-- | Estado inicial a partir de un mundo cargado.
initialAppState :: SpriteCatalog -> World -> AppState
initialAppState sprites world =
  AppState
    { appGameState = initialGameState defaultConfig world
    , appSprites = sprites
    , appRenderFrame = 0
    , appKeysHeld = noKeys
    , appJumpPrev = False
    , appAttackPrev = False
    , appShowHitboxes = True
    }

drawFrame :: AppState -> IO Picture
drawFrame state =
  pure
    ( renderFrame
        (appSprites state)
        (appRenderFrame state)
        (appShowHitboxes state)
        (gameViewFromState defaultConfig (appGameState state))
    )

handleEvent :: Event -> AppState -> IO AppState
handleEvent (EventKey (SpecialKey KeyEsc) Gloss.Down _ _) _ = exitSuccess
handleEvent (EventKey (SpecialKey KeyF1) Gloss.Down _ _) state =
  pure state{appShowHitboxes = not (appShowHitboxes state)}
handleEvent event state =
  pure state{appKeysHeld = handleKeyEvent event (appKeysHeld state)}

advanceFrame :: Float -> AppState -> IO AppState
advanceFrame dt state = do
  let dt' = capDeltaTime dt
      frozen = isFrozen dt'
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
          , appRenderFrame =
              if frozen
                then appRenderFrame state
                else (appRenderFrame state + 1) `mod` 1000000
          , appJumpPrev = if frozen then appJumpPrev state else jumpPrev
          , appAttackPrev = if frozen then appAttackPrev state else attackPrev
          }
