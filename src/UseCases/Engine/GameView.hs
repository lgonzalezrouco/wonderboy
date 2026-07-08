{- | Snapshot de solo lectura para el adaptador de renderizado.

'GameView' es el DTO de salida del puerto primario: la aplicación define qué
ofrece al frontend; el adaptador de Gloss lo consume sin importar 'GameMonad'
ni 'GameState' directamente.
-}
module UseCases.Engine.GameView (
  GameView (..),
  gameViewFromState,
  bossHealthFromWorld,
)
where

import Control.Monad (guard)

import Domain.Logic.BossArena (bossArenaSealed, bossArenaWallPlatforms, bossArenaWallsActive, playerWithinBossArena)
import Domain.Logic.LevelFlow (findLivingBoss, showBossExitHint, showExitScoreHint)
import Domain.Logic.MeleeSwing (meleeHitboxWhenImpact)
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

-- | Snapshot para HUD y capa de mundo en un frame.
data GameView = GameView
  { gvWorld :: World
  , gvLives :: Lives
  , gvPhase :: GamePhase
  , gvMaxHealth :: Health
  -- ^ Salud máxima (run-wide, derivada de config): cuántos pips dibuja el HUD.
  , gvStartingLives :: Lives
  -- ^ Vidas iniciales (run-wide, derivada de config): cuántos iconos dibuja el HUD.
  , gvScore :: Score
  -- ^ Puntuación del nivel actual (run-state, proyectada desde 'GameState').
  , gvBossHealth :: Maybe BossHealth
  -- ^ Salud del jefe vivo cuando el jugador está en la arena.
  , gvCombatParams :: CombatParams
  -- ^ Parámetros de combate para postura y animación de cue de ataque en el adaptador.
  , gvLevelIndex :: Int
  -- ^ Nivel actual del run (1–3).
  , gvExitScoreHint :: Maybe (Score, Score)
  -- ^ Puntuación actual y mínima cuando el jugador está en la salida sin alcanzar el umbral.
  , gvBossExitHint :: Bool
  -- ^ Hint de jefe vivo en la salida con puntuación suficiente.
  , gvBossArenaSealed :: Bool
  -- ^ Arena de jefe activa: jugador confinado hasta derrotar al jefe.
  , gvMeleeHitbox :: Maybe Aabb
  -- ^ Hitbox de melee pre-calculada (solo mientras el ataque está activo).
  --   El adaptador de debug la dibuja sin necesitar 'Domain.Logic.Combat'.
  , gvBossArenaWalls :: [Platform]
  -- ^ Paredes visibles de la arena de jefe (cajas idénticas a la colisión).
  --   Vacío si el jugador no está dentro o el jefe ya fue derrotado.
  }
  deriving (Eq, Show)

{- | Proyección para el adaptador de renderizado.

Recibe 'GameConfig' para que el HUD derive sus máximos (salud, vidas iniciales) de
la configuración. Pre-calcula 'gvMeleeHitbox' y 'gvBossArenaWalls' para que el
adaptador de renderizado no importe lógica de dominio.
-}
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
        , gvMeleeHitbox = meleeHitboxWhenImpact combatParams p
        , gvBossArenaWalls =
            maybe
              []
              (\arena -> if bossArenaWallsActive w then bossArenaWallPlatforms arena else [])
              (worldBossArena w)
        }

-- | Proyecta salud del jefe vivo para el HUD (como máximo un jefe por nivel).
bossHealthFromWorld :: World -> Maybe BossHealth
bossHealthFromWorld w = do
  guard (playerWithinBossArena w)
  e <- findLivingBoss w
  pure (bossHealth (enemyHealth e) (enemyMaxHealth e))
