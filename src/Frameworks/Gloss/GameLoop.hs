-- | Bucle de juego Gloss: orquesta adaptadores y 'updateGame' (M8).
module Frameworks.Gloss.GameLoop (
  runGame,

  -- * Catálogo demo (tres niveles; el motor acepta listas de cualquier longitud)
  demoLevelPaths,
)
where

import Adapters.BehaviourResolver (resolveLevelIO)
import Adapters.Gloss.Config (backgroundColor, windowHeight, windowWidth)
import Adapters.Gloss.Input (KeyState, buildInput, handleKeyEvent, noKeys)
import Adapters.Gloss.Rendering (renderFrame)
import Adapters.Gloss.Sprites (SpriteCatalog, loadSpriteCatalog)
import Adapters.Gloss.Time (capDeltaTime)
import Adapters.LevelFile (readLevelFile)
import Adapters.LevelGenerator (generateCatalogIO)
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
import Paths_wonderboy_hs (getDataFileName)
import System.Environment (lookupEnv)
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
import UseCases.LoadLevel (decodeLevelDefinition, worldFromDefinition)
import UseCases.UpdateGame (updateGame)

import Data.Text qualified as T

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
  -- ^ Configuración del run (incluye el conteo de niveles del catálogo).
  , appLevelDefs :: [LevelDefinition]
  -- ^ Catálogo de niveles ya resuelto en memoria (generados por IA o leídos de
  -- disco). Pre-cargarlos al arrancar evita 'IO' de generación entre niveles:
  -- cada transición solo aplica el build puro ('worldFromDefinition') sobre la
  -- definición ya validada en su índice.
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

{- | Arranca la ventana Gloss con un catálogo de niveles arbitrario (>= 1 ruta).

Pre-carga TODO el catálogo de definiciones de nivel antes de abrir la ventana,
porque la generación por IA es 'IO' y costosa: hacerla una vez al inicio (y no en
cada transición) deja el resto del bucle puro sobre definiciones ya validadas en
memoria.

Las @paths@ siguen siendo el catálogo de archivos fijos: fijan el conteo de
niveles del run y sirven de fallback granular para cualquier nivel que la IA no
pueda generar.
-}
runGameWith :: [FilePath] -> IO ()
runGameWith paths = do
  let cfg = configForLevelCatalog paths
  defs <- buildLevelCatalog paths
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

{- | Arma el catálogo de definiciones de nivel del run, con o sin generación IA.

Si @WONDERBOY_GENERATE_LEVELS@ está seteada (cualquier valor no vacío), pide el
catálogo a la IA con el tema opcional de @WONDERBOY_WORLD_PROMPT@ y empareja el
resultado con las @paths@ por índice: cada nivel generado ('Just') se usa tal
cual y cada hueco ('Nothing') cae al archivo fijo correspondiente (fallback
granular). Si la generación está apagada, lee las definiciones de los archivos.

Tras armar el catálogo, corre el behaviour-resolver sobre cada definición
('resolveLevelIO'): es un no-op para los niveles generados (que no traen
@behaviourHint@) y resuelve las pistas autorales de los niveles leídos de disco.
-}
buildLevelCatalog :: [FilePath] -> IO [LevelDefinition]
buildLevelCatalog paths = do
  genOn <- lookupEnv "WONDERBOY_GENERATE_LEVELS"
  theme <- lookupEnv "WONDERBOY_WORLD_PROMPT"
  defs <-
    if isEnabled genOn
      then do
        -- Generación activa: una consulta a la IA por nivel; el tema (si lo hay)
        -- se propaga a los tres perfiles dentro de `generateCatalogIO`.
        generated <- generateCatalogIO (T.pack <$> theme)
        traverse (uncurry resolveSlot) (zip paths (padTo (length paths) generated))
      else
        -- Generación apagada: comportamiento previo, todo desde archivos fijos.
        traverse loadDefFromFile paths
  -- El behaviour-resolver corre igual en ambos caminos: no-op para generados,
  -- resuelve hints para los leídos de disco.
  traverse resolveLevelIO defs
 where
  -- ¿La env var de activación está presente y no vacía?
  isEnabled = maybe False (not . null)

  resolveSlot :: FilePath -> Maybe LevelDefinition -> IO LevelDefinition
  resolveSlot _ (Just def) = pure def
  resolveSlot path Nothing = loadDefFromFile path

{- | Rellena la lista de niveles generados hasta el largo del catálogo de
archivos, completando con 'Nothing' (que dispara el fallback granular).

La IA debería devolver tantos niveles como perfiles, pero protegemos contra una
lista más corta para que el @zip@ con las @paths@ cubra todos los índices.
-}
padTo :: Int -> [Maybe a] -> [Maybe a]
padTo n xs = take n (xs ++ repeat Nothing)

{- | Lee y decodifica una definición de nivel desde un archivo del catálogo.

Resuelve la ruta empaquetada por Cabal, lee el archivo y lo decodifica. A
diferencia de la generación (que degrada a fallback), un archivo fijo ilegible o
malformado es un error de configuración del juego: se sale con 'exitWithError'.
-}
loadDefFromFile :: FilePath -> IO LevelDefinition
loadDefFromFile relPath = do
  path <- getDataFileName relPath
  readResult <- readLevelFile path
  case readResult of
    Left err -> exitWithError err
    Right txt ->
      case decodeLevelDefinition txt of
        Left (GameError err) -> exitWithError err
        Right def -> pure def

{- | Construye el mundo desde una definición del catálogo pre-cargado (índice
0-based).

Toma la 'LevelDefinition' ya resuelta en memoria y aplica el build puro
('worldFromDefinition'). Un índice fuera de rango o un build fallido son errores
de configuración: se sale con 'exitWithError'.
-}
loadWorldFromCatalog :: [LevelDefinition] -> Int -> IO World
loadWorldFromCatalog defs idx =
  case defs !!? idx of
    Nothing -> exitWithError ("invalid level index: " ++ show idx)
    Just def ->
      case worldFromDefinition def of
        Left (GameError err) -> exitWithError err
        Right world -> pure world

exitWithError :: String -> IO a
exitWithError err = hPutStrLn stderr ("Error: " ++ err) >> exitFailure

(!!?) :: [a] -> Int -> Maybe a
(!!?) xs n
  | n < 0 = Nothing
  | otherwise = go n xs
 where
  go _ [] = Nothing
  go 0 (x : _) = Just x
  go k (_ : xs') = go (k - 1) xs'

-- | Estado inicial a partir de un mundo cargado y el catálogo pre-cargado.
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
