module UseCases.Engine.GameView (
  GameView (..),
  gameViewFromState,
  bossHealthFromWorld,
)
where

import Control.Monad (guard)

import Domain.Logic.BossArena (bossArenaSealed, bossArenaWallPlatforms, bossArenaWallsActive, playerWithinBossArena)
import Domain.Logic.LevelFlow (findLivingBoss, showBossExitHint, showExitScoreHint)
import Domain.Logic.MeleeSwing (meleeHitboxDuringSwing)
import Domain.Model.Enemy (enemyHealth, enemyMaxHealth)
import Domain.Model.GamePhase (GamePhase)
import Domain.Model.Platform (Platform)
import Domain.Model.World (World (..), worldBossArena)
import Domain.ValueObjects.Aabb (Aabb)
import Domain.ValueObjects.BossHealth (BossHealth, bossHealth)
import Domain.ValueObjects.CombatParams (CombatParams)
import Domain.ValueObjects.Health (Health)
import Domain.ValueObjects.Lives (Lives)
import Domain.ValueObjects.Score (Score)
import UseCases.Engine.GameConfig (GameConfig (..), combatParamsFromConfig)
import UseCases.Engine.GameState (GameState (..))

data GameView = GameView
  { gvWorld :: World
  , gvLives :: Lives
  , gvPhase :: GamePhase
  , gvMaxHealth :: Health
  , gvStartingLives :: Lives
  , gvScore :: Score
  , gvBossHealth :: Maybe BossHealth
  -- ^ Presente solo mientras el jugador está dentro de la arena y un boss sigue vivo.
  , gvCombatParams :: CombatParams
  , gvLevelIndex :: Int
  -- ^ Índice (base 1) del nivel actual dentro del run.
  , gvExitScoreHint :: Maybe (Score, Score)
  -- ^ Just (puntaje actual, mínimo requerido) mientras el jugador está en la salida por debajo del umbral.
  , gvBossExitHint :: Bool
  , gvBossArenaSealed :: Bool
  , gvMeleeHitbox :: Maybe Aabb
  -- ^ Presente durante toda la ventana de swing del ataque (para visualización).
  , gvBossArenaWalls :: [Platform]
  -- ^ Vacío salvo que el jugador esté encerrado en la arena del boss con el boss aún vivo.
  }
  deriving (Eq, Show)

gameViewFromState :: GameConfig -> GameState -> GameView
gameViewFromState cfg gs =
  let w = gsWorld gs
      s = gsScore gs
      combatParams = combatParamsFromConfig cfg
      p = worldPlayer w
   in GameView
        { gvWorld = w
        , gvLives = gsLives gs
        , gvPhase = gsPhase gs
        , gvMaxHealth = gcMaxHealth cfg
        , gvStartingLives = gcStartingLives cfg
        , gvScore = s
        , gvBossHealth = bossHealthFromWorld w
        , gvCombatParams = combatParams
        , gvLevelIndex = gsLevelIndex gs
        , gvExitScoreHint =
            if showExitScoreHint s w
              then Just (s, worldMinScore w)
              else Nothing
        , gvBossExitHint = showBossExitHint s w
        , gvBossArenaSealed = bossArenaSealed w
        , gvMeleeHitbox = meleeHitboxDuringSwing combatParams p
        , gvBossArenaWalls =
            maybe
              []
              (\arena -> if bossArenaWallsActive w then bossArenaWallPlatforms arena else [])
              (worldBossArena w)
        }

bossHealthFromWorld :: World -> Maybe BossHealth
bossHealthFromWorld w = do
  guard (playerWithinBossArena w)
  e <- findLivingBoss w
  pure (bossHealth (enemyHealth e) (enemyMaxHealth e))
