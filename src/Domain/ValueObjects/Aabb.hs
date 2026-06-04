{- | Caja alineada a los ejes (AABB) en el espacio del juego.

Representa un rectángulo axis-aligned por sus bordes min/max en píxeles lógicos.
Convención de ejes: X crece a la derecha, Y crece hacia arriba.
-}
module Domain.ValueObjects.Aabb (
  -- * Tipo
  Aabb (..),

  -- * Construcción
  aabbFromBottomLeft,
  aabbFromBottomCenter,

  -- * Predicados
  aabbOverlaps,
)
where

import Domain.ValueObjects.Position (Position, posX, posY)

{- | Rectángulo axis-aligned: esquina inferior izquierda y superior derecha.

Los bordes son inclusivos para overlap tests: dos cajas que se tocan
en un borde se consideran superpuestas.
-}
data Aabb = Aabb
  { aabbMinX :: Float
  , aabbMinY :: Float
  , aabbMaxX :: Float
  , aabbMaxY :: Float
  }
  deriving (Eq, Show)

{- | Caja con esquina inferior izquierda en @pos@ y tamaño @width@ × @height@.

@height@ crece hacia arriba: @aabbMaxY = posY pos + height@.
-}
aabbFromBottomLeft :: Position -> Float -> Float -> Aabb
aabbFromBottomLeft pos width height =
  Aabb
    { aabbMinX = posX pos
    , aabbMinY = posY pos
    , aabbMaxX = posX pos + width
    , aabbMaxY = posY pos + height
    }

{- | Caja con @pos@ en el centro inferior (pies del jugador).

@width@ se extiende ±width/2 en X; @height@ crece hacia arriba desde los pies.
-}
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

-- | 'True' si las dos cajas tienen intersección no vacía (bordes inclusivos).
aabbOverlaps :: Aabb -> Aabb -> Bool
aabbOverlaps a b =
  aabbMinX a <= aabbMaxX b
    && aabbMaxX a >= aabbMinX b
    && aabbMinY a <= aabbMaxY b
    && aabbMaxY a >= aabbMinY b
