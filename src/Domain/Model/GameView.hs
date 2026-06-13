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
  }
  deriving (Eq, Show)
