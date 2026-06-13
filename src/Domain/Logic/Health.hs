{- | Reglas puras de daño, pérdida de vida y respawn (M9).

'applyDamage' sólo reduce salud; 'resolveLifeLoss' aplica respawn o game over
cuando la salud llega a cero. Fuentes de daño futuras (M10) llaman sólo a
'applyDamage'; la resolución centralizada vive al final de 'advanceFrame'.
-}
module Domain.Logic.Health (
  -- * Daño y muerte
  applyDamage,
  resolveLifeLoss,

  -- * Respawn
  respawnPlayer,
)
where

import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.Player (
  Player (..),
  playerHealth,
  playerMaxHealth,
 )
import Domain.Model.World (World (..))
import Domain.ValueObjects.Position (Position)
import Domain.ValueObjects.Velocity (velocity)

-- | Resta @amount@ de salud del jugador, sin bajar de 0 ni perder vidas aquí.
applyDamage :: Int -> World -> World
applyDamage amount w
  | amount <= 0 = w
  | otherwise =
      let p = worldPlayer w
          h = max 0 (playerHealth p - amount)
       in w {worldPlayer = p {playerHealth = h}}

-- | Si la salud es 0 en fase 'Playing', descuenta una vida y respawnea o termina.
resolveLifeLoss :: World -> World
resolveLifeLoss w
  | worldPhase w /= Playing = w
  | playerHealth (worldPlayer w) > 0 = w
  | worldLives w <= 1 =
      w
        { worldLives = 0
        , worldPhase = GameOver
        }
  | otherwise =
      w
        { worldLives = worldLives w - 1
        , worldPlayer = respawnPlayer (worldSpawnPoint w) (worldPlayer w)
        }

-- | Coloca al jugador en el spawn con salud plena y sin movimiento.
respawnPlayer :: Position -> Player -> Player
respawnPlayer spawn p =
  p
    { playerPos = spawn
    , playerVel = velocity 0 0
    , playerOnGround = False
    , playerHealth = playerMaxHealth
    }
