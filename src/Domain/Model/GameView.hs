{- | Vista de solo lectura para renderizado (mundo + estado run-wide).

Evita que @Adapters/@ importen @UseCases.GameMonad@: el framework
construye esta vista desde 'GameState' antes de dibujar.
-}
module Domain.Model.GameView (
  GameView (..),
)
where

import Domain.Model.GamePhase (GamePhase)
import Domain.Model.World (World)
import Domain.ValueObjects.BossHealth (BossHealth (..))
import Domain.ValueObjects.CombatParams (CombatParams)
import Domain.ValueObjects.Health (Health)
import Domain.ValueObjects.Lives (Lives)
import Domain.ValueObjects.Score (Score)

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
  -- ^ Salud del jefe vivo, si hay uno en el nivel.
  , gvCombatParams :: CombatParams
  -- ^ Parámetros de combate (proyectados desde config) para debug de hitboxes en el adaptador.
  }
  deriving (Eq, Show)
