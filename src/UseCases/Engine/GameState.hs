{- | Estado mutable del run: mundo de nivel + vidas + fase.

Ver @docs\/adr\/0012-gamestate-run-snapshot.md@ para el razonamiento de diseño.
-}
module UseCases.Engine.GameState (
  GameState (..),
  initialGameState,
  startLevel,
  advanceAfterLevelComplete,
  restartRun,
)
where

-- Grupo 1 — stdlib / base
import GHC.Generics (Generic)

-- Grupo 3 — proyecto
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.Player (spawnPlayer)
import Domain.Model.World (World (..))
import Domain.ValueObjects.Lives (Lives)
import Domain.ValueObjects.Score (Score, score)
import UseCases.Engine.GameConfig (GameConfig (..))

{- | Estado mutable del juego: mundo de nivel + estado run-wide.

Contiene el 'World' del nivel actual más vidas y fase de la partida.
-}
data GameState = GameState
  { gsWorld :: World
  , gsLives :: Lives
  , gsPhase :: GamePhase
  , gsScore :: Score
  -- ^ Puntuación del nivel actual; se reinicia al cargar un nivel.
  , gsLevelIndex :: Int
  -- ^ Posición 1-based del nivel actual dentro del run.
  }
  deriving (Eq, Show, Generic)

-- | Estado inicial de una partida nueva a partir de un mundo de nivel.
initialGameState :: GameConfig -> World -> GameState
initialGameState cfg = startLevel cfg (gcStartingLives cfg) 1

-- | Carga un nivel en el run conservando vidas y reiniciando puntuación y salud.
startLevel :: GameConfig -> Lives -> Int -> World -> GameState
startLevel cfg runLives levelIndex w =
  GameState
    { gsWorld = w{worldPlayer = spawnPlayer (gcMaxHealth cfg) (worldSpawnPoint w)}
    , gsLives = runLives
    , gsPhase = Playing
    , gsScore = score 0
    , gsLevelIndex = levelIndex
    }

-- | Avanza al siguiente nivel tras confirmar 'LevelComplete'.
advanceAfterLevelComplete :: GameConfig -> GameState -> World -> GameState
advanceAfterLevelComplete cfg gs =
  startLevel cfg (gsLives gs) (gsLevelIndex gs + 1)

-- | Reinicia el run desde el nivel 1 con vidas iniciales.
restartRun :: GameConfig -> World -> GameState
restartRun cfg = startLevel cfg (gcStartingLives cfg) 1
