module Domain.Model.Platform (
  Platform (..),
  platform,
  platformAabb,
)
where

import Domain.ValueObjects.Aabb (Aabb, aabbFromBottomLeft)
import Domain.ValueObjects.Position (Position)

data Platform = Platform
  { platformPos :: Position
  -- ^ Esquina inferior izquierda de la caja sólida.
  , platformWidth :: Float
  , platformHeight :: Float
  }
  deriving (Eq, Show)

platform :: Position -> Float -> Float -> Platform
platform pos width height =
  Platform
    { platformPos = pos
    , platformWidth = width
    , platformHeight = height
    }

platformAabb :: Platform -> Aabb
platformAabb p =
  aabbFromBottomLeft
    (platformPos p)
    (platformWidth p)
    (platformHeight p)
