{- | Modelo del jugador dentro del mundo del juego.

El jugador es una entidad: su estado cambia frame a frame (posición, velocidad,
vida) pero conserva su identidad conceptual.
-}
module Domain.Model.Player (
  -- * Tipo
  Player (..),

  -- * Caja de colisión
  playerWidth,
  playerHeight,
  playerAabb,

  -- * Construcción
  spawnPlayer,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Aabb (Aabb, aabbFromBottomCenter)
import Domain.ValueObjects.Facing (Facing (..))
import Domain.ValueObjects.Frames (Frames, noFrames)
import Domain.ValueObjects.Health (Health)
import Domain.ValueObjects.Position (Position)
import Domain.ValueObjects.Velocity (Velocity, velocity)

-- | Estado del jugador en un frame dado.
data Player = Player
  { playerPos :: Position
  , playerVel :: Velocity
  , playerOnGround :: Bool
  -- ^ Controla si puede iniciar un salto (apoyado sobre una superficie).
  , playerHealth :: Health
  -- ^ Puntos de vida; 0 = muerto.
  , playerFacing :: Facing
  -- ^ Orientación horizontal (dirección del alcance de melee).
  , playerAttackFrames :: Frames
  -- ^ Frames restantes de ventana de melee; 0 = sin ataque activo.
  , playerInvincibilityFrames :: Frames
  -- ^ Frames de invencibilidad restantes; 0 = vulnerable a contacto enemigo.
  , playerThrowCooldownFrames :: Frames
  -- ^ Frames restantes antes de poder lanzar de nuevo; 0 = listo.
  }
  deriving (Eq, Show, Generic)

playerWidth :: Float
playerWidth = 32.0

playerHeight :: Float
playerHeight = 48.0

-- | Caja de colisión del jugador: @playerPos@ es el centro inferior (pies).
playerAabb :: Player -> Aabb
playerAabb p =
  aabbFromBottomCenter (playerPos p) playerWidth playerHeight

-- | Crea un jugador en su posición de spawn, en reposo y con vida completa.
spawnPlayer :: Health -> Position -> Player
spawnPlayer maxHealth pos =
  Player
    { playerPos = pos
    , playerVel = velocity 0 0
    , playerOnGround = False
    , playerHealth = maxHealth
    , playerFacing = FacingRight
    , playerAttackFrames = noFrames
    , playerInvincibilityFrames = noFrames
    , playerThrowCooldownFrames = noFrames
    }
