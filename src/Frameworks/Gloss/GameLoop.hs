module Frameworks.Gloss.GameLoop (
  runGame,
  demoLevelPaths,
)
where

import Adapters.BootstrapRunIO (bootstrapCatalogIO)
import Adapters.Gloss.Config (backgroundColor, windowHeight, windowWidth)
import Adapters.Gloss.Input (KeyState, buildInput, handleKeyEvent, noKeys)
import Adapters.Gloss.Rendering (renderFrame)
import Adapters.Gloss.Sprites (SpriteCatalog, loadSpriteCatalog)
import Adapters.Gloss.Time (capDeltaTime)
import Domain.Model.GamePhase (GamePhase (..), isSimulationFrozen)
import Domain.Model.LevelDefinition (LevelDefinition)
import Domain.Model.World (World)
import Domain.ValueObjects.DeltaTime (isFrozen)
import Graphics.Gloss (Display (InWindow), Picture)
import Graphics.Gloss.Interface.IO.Game (
  Event (..),
  Key (..),
  SpecialKey (KeyEnter, KeyEsc, KeyF1, KeySpace),
  playIO,
 )
import Graphics.Gloss.Interface.IO.Game qualified as Gloss (KeyState (Down))
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import UseCases.GameMonad (
  GameConfig,
  GameError (..),
  GameState,
  advanceAfterLevelComplete,
  configForLevelCatalog,
  gameViewFromState,
  gsLevelIndex,
  gsPhase,
  initialGameState,
  restartRun,
  runGameM,
 )
import UseCases.LoadLevel (worldFromCatalog)
import UseCases.RunLayout (layoutPaths)
import UseCases.UpdateGame (updateGame)

{- | Las rutas de los niveles de la partida, tomadas de 'UseCases.RunLayout.layoutPaths' (la única
fuente de verdad). Para cambiar qué niveles se cargan, edita el layout ahí, no esta lista.
-}
demoLevelPaths :: [FilePath]
demoLevelPaths = layoutPaths

data AppState = AppState
  { appConfig :: GameConfig
  , appLevelDefs :: [LevelDefinition]
  -- ^ Catálogo precargado al inicio, así no corre IO de generación de niveles entre niveles.
  , appGameState :: GameState
  , appSprites :: SpriteCatalog
  , appRenderFrame :: Int
  , appKeysHeld :: KeyState
  , appJumpPrev :: Bool
  , appAttackPrev :: Bool
  , appThrowPrev :: Bool
  , appShowHitboxes :: Bool
  }

runGame :: IO ()
runGame = runGameWith demoLevelPaths

runGameWith :: [FilePath] -> IO ()
runGameWith paths = do
  let cfg = configForLevelCatalog (length paths)
  defs <- bootstrapCatalogIO paths
  world <- loadWorldFromCatalog defs 0
  sprites <- loadSpriteCatalog
  playIO
    (InWindow "Wonder Boy" (windowWidth, windowHeight) (100, 100))
    backgroundColor
    60
    (initialAppState cfg defs sprites world)
    drawFrame
    handleEvent
    advanceFrame

loadWorldFromCatalog :: [LevelDefinition] -> Int -> IO World
loadWorldFromCatalog defs idx =
  case worldFromCatalog defs idx of
    Left (GameError err) -> exitWithError err
    Right world -> pure world

exitWithError :: String -> IO a
exitWithError err = hPutStrLn stderr ("Error: " ++ err) >> exitFailure

initialAppState :: GameConfig -> [LevelDefinition] -> SpriteCatalog -> World -> AppState
initialAppState cfg defs sprites world =
  AppState
    { appConfig = cfg
    , appLevelDefs = defs
    , appGameState = initialGameState cfg world
    , appSprites = sprites
    , appRenderFrame = 0
    , appKeysHeld = noKeys
    , appJumpPrev = False
    , appAttackPrev = False
    , appThrowPrev = False
    , appShowHitboxes = False
    }

drawFrame :: AppState -> IO Picture
drawFrame state =
  pure
    ( renderFrame
        (appSprites state)
        (appRenderFrame state)
        (appShowHitboxes state)
        (gameViewFromState (appConfig state) (appGameState state))
    )

handleEvent :: Event -> AppState -> IO AppState
handleEvent (EventKey (SpecialKey KeyEsc) Gloss.Down _ _) _ = exitSuccess
handleEvent (EventKey (SpecialKey KeyF1) Gloss.Down _ _) state =
  pure state{appShowHitboxes = not (appShowHitboxes state)}
handleEvent event state
  | isMenuConfirmDown event
  , isSimulationFrozen (gsPhase (appGameState state)) =
      handleConfirm state
  | otherwise =
      pure state{appKeysHeld = handleKeyEvent event (appKeysHeld state)}

-- | Enter siempre confirma en los menús. Space confirma solo fuera de 'Playing' (dentro del juego Space es atacar).
isMenuConfirmDown :: Event -> Bool
isMenuConfirmDown (EventKey key Gloss.Down _ _) =
  case key of
    Char '\r' -> True
    SpecialKey KeyEnter -> True
    Char ' ' -> True
    SpecialKey KeySpace -> True
    _ -> False
isMenuConfirmDown _ = False

resetInputState :: AppState -> AppState
resetInputState state =
  state
    { appKeysHeld = noKeys
    , appJumpPrev = False
    , appAttackPrev = False
    , appThrowPrev = False
    }

handleConfirm :: AppState -> IO AppState
handleConfirm state =
  case gsPhase (appGameState state) of
    Playing -> pure state
    LevelComplete -> do
      let defs = appLevelDefs state
          nextIdx = gsLevelIndex (appGameState state)
      world <- loadWorldFromCatalog defs nextIdx
      pure $
        resetInputState
          state
            { appGameState =
                advanceAfterLevelComplete (appConfig state) (appGameState state) world
            }
    _ -> restartFromLevelOne state

restartFromLevelOne :: AppState -> IO AppState
restartFromLevelOne state = do
  world <- loadWorldFromCatalog (appLevelDefs state) 0
  pure $
    resetInputState
      state{appGameState = restartRun (appConfig state) world}

advanceFrame :: Float -> AppState -> IO AppState
advanceFrame dt state = do
  let cfg = appConfig state
      dt' = capDeltaTime dt
      frozen = isFrozen dt'
      (input, jumpPrev, attackPrev, throwPrev) =
        buildInput
          (appKeysHeld state)
          (appJumpPrev state)
          (appAttackPrev state)
          (appThrowPrev state)
  case runGameM cfg (appGameState state) (updateGame dt' input) of
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
          , appThrowPrev = if frozen then appThrowPrev state else throwPrev
          }
