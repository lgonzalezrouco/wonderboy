module Domain.ValueObjects.PhysicsParams (
  PhysicsParams (..),
  physicsParams,
)
where

import GHC.Generics (Generic)

data PhysicsParams = PhysicsParams
  { ppGravity :: Float
  -- ^ px/s^2, se resta de vy en cada frame
  , ppMoveSpeed :: Float
  -- ^ px/s, tope horizontal mientras se mantiene el input
  , ppJumpSpeed :: Float
  -- ^ px/s hacia arriba, impulso inicial del salto
  }
  deriving (Eq, Show, Generic)

physicsParams :: Float -> Float -> Float -> PhysicsParams
physicsParams g move jump =
  PhysicsParams
    { ppGravity = g
    , ppMoveSpeed = move
    , ppJumpSpeed = jump
    }
