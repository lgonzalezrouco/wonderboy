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

-- | Snapshot para HUD y capa de mundo en un frame.
data GameView = GameView
  { gvWorld :: World
  , gvLives :: Int
  , gvPhase :: GamePhase
  , gvMaxHealth :: Int
  -- ^ Salud máxima (run-wide, derivada de config): cuántos pips dibuja el HUD.
  , gvStartingLives :: Int
  -- ^ Vidas iniciales (run-wide, derivada de config): cuántos iconos dibuja el HUD.
  }
  deriving (Eq, Show)
