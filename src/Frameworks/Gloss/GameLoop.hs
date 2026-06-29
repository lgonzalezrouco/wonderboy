-- | Bucle de juego Gloss: orquesta adaptadores y 'updateGame' (M8).
module Frameworks.Gloss.GameLoop (
  runGame,

  -- * Catálogo demo (tres niveles; el motor acepta listas de cualquier longitud)
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
import UseCases.UpdateGame (updateGame)

-- | Catálogo del demo (tres niveles). Añadir rutas aquí o pasar otra lista a 'runGameWith'.
demoLevelPaths :: [FilePath]
demoLevelPaths =
  [ "levels/level1.json"
  , "levels/level2.json"
  , "levels/level3.json"
  ]

-- | Estado de la aplicación Gloss (no es estado de dominio).
data AppState = AppState
  { appConfig :: GameConfig
  , appLevelDefs :: [LevelDefinition]
  -- ^ Catálogo pre-cargado al arrancar (evita 'IO' de generación entre niveles).
  , appGameState :: GameState
  , appSprites :: SpriteCatalog
  , appRenderFrame :: Int
  , appKeysHeld :: KeyState
  , appJumpPrev :: Bool
  , appAttackPrev :: Bool
  , appThrowPrev :: Bool
  , appShowHitboxes :: Bool
  }

-- | Arranca la ventana Gloss con el catálogo demo de tres niveles.
runGame :: IO ()
runGame = runGameWith demoLevelPaths

-- | Arranca Gloss; pre-carga el catálogo (generación IA o archivos fijos).
runGameWith :: [FilePath] -> IO ()
runGameWith paths = do
  let cfg = configForLevelCatalog paths
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

-- | Estado inicial a partir de un mundo cargado.
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

-- | Enter confirma en menús; Space también fuera de 'Playing' (ataque usa Space en juego).
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
