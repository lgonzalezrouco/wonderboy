module Domain.ValueObjects.Aabb (
  Aabb (..),
  aabbFromBottomLeft,
  aabbFromBottomCenter,
  aabbOverlaps,
)
where

import Domain.ValueObjects.Position (Position, posX, posY)

data Aabb = Aabb
  { aabbMinX :: Float
  , aabbMinY :: Float
  , aabbMaxX :: Float
  , aabbMaxY :: Float
  }
  deriving (Eq, Show)

aabbFromBottomLeft :: Position -> Float -> Float -> Aabb
aabbFromBottomLeft pos width height =
  Aabb
    { aabbMinX = posX pos
    , aabbMinY = posY pos
    , aabbMaxX = posX pos + width
    , aabbMaxY = posY pos + height
    }

-- | Caja anclada de modo que pos sea el centro-inferior (los pies del jugador). Se extiende width/2 hacia cada lado y crece height hacia arriba.
aabbFromBottomCenter :: Position -> Float -> Float -> Aabb
aabbFromBottomCenter pos width height =
  let halfW = width / 2
      x = posX pos
      y = posY pos
   in Aabb
        { aabbMinX = x - halfW
        , aabbMinY = y
        , aabbMaxX = x + halfW
        , aabbMaxY = y + height
        }

-- | Test de solapamiento con bordes inclusivos: cajas que se tocan justo en un borde cuentan como solapadas.
aabbOverlaps :: Aabb -> Aabb -> Bool
aabbOverlaps a b =
  aabbMinX a <= aabbMaxX b
    && aabbMaxX a >= aabbMinX b
    && aabbMinY a <= aabbMaxY b
    && aabbMaxY a >= aabbMinY b
