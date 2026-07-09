module UseCases.Engine.GameState (
  GameState (..),
  initialGameState,
  startLevel,
  advanceAfterLevelComplete,
  restartRun,
)
where

import GHC.Generics (Generic)

import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.Player (spawnPlayer)
import Domain.Model.World (World (..))
import Domain.ValueObjects.Lives (Lives)
import Domain.ValueObjects.Score (Score, score)
import UseCases.Engine.GameConfig (GameConfig (..))

data GameState = GameState
  { gsWorld :: World
  , gsLives :: Lives
  , gsPhase :: GamePhase
  , gsScore :: Score
  , gsLevelIndex :: Int
  -- ^ Posición (base 1) del nivel actual dentro del run.
  }
  deriving (Eq, Show, Generic)

initialGameState :: GameConfig -> World -> GameState
initialGameState cfg = startLevel cfg (gcStartingLives cfg) 1

startLevel :: GameConfig -> Lives -> Int -> World -> GameState
startLevel cfg runLives levelIndex w =
  GameState
    { gsWorld = w{worldPlayer = spawnPlayer (gcMaxHealth cfg) (worldSpawnPoint w)}
    , gsLives = runLives
    , gsPhase = Playing
    , gsScore = score 0
    , gsLevelIndex = levelIndex
    }

advanceAfterLevelComplete :: GameConfig -> GameState -> World -> GameState
advanceAfterLevelComplete cfg gs =
  startLevel cfg (gsLives gs) (gsLevelIndex gs + 1)

restartRun :: GameConfig -> World -> GameState
restartRun cfg = startLevel cfg (gcStartingLives cfg) 1
