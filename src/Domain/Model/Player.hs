module Domain.Model.Player (
  Player (..),
  playerWidth,
  playerHeight,
  playerAabb,
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

data Player = Player
  { playerPos :: Position
  -- ^ Centro inferior de la caja de colisión (los pies del jugador).
  , playerVel :: Velocity
  , playerOnGround :: Bool
  , playerHealth :: Health
  , playerFacing :: Facing
  , playerAttackFrames :: Frames
  , playerInvincibilityFrames :: Frames
  , playerThrowCooldownFrames :: Frames
  }
  deriving (Eq, Show, Generic)

playerWidth :: Float
playerWidth = 32.0

playerHeight :: Float
playerHeight = 48.0

playerAabb :: Player -> Aabb
playerAabb p =
  aabbFromBottomCenter (playerPos p) playerWidth playerHeight

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
