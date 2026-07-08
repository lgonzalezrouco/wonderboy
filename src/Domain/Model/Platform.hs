{- | Plataforma estática del nivel (sólido con colisión AABB).

Las plataformas son geometría del mundo: no tienen velocidad ni identidad
como las entidades.
-}
module Domain.Model.Platform (
  -- * Tipo
  Platform (..),

  -- * Construcción
  platform,

  -- * Geometría
  platformAabb,
)
where

import Domain.ValueObjects.Aabb (Aabb, aabbFromBottomLeft)
import Domain.ValueObjects.Position (Position)

{- | Segmento sólido del nivel: caja con esquina inferior izquierda y tamaño.

@platformPos@ es la esquina inferior izquierda; @platformHeight@ crece hacia arriba.
-}
data Platform = Platform
  { platformPos :: Position
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

-- | Caja de colisión de la plataforma (bottom-left anchor).
platformAabb :: Platform -> Aabb
platformAabb p =
  aabbFromBottomLeft
    (platformPos p)
    (platformWidth p)
    (platformHeight p)
