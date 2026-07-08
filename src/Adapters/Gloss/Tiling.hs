module Adapters.Gloss.Tiling (
  tilesToCover,
)
where

-- Restamos tileFitEpsilon antes del ceiling: de lo contrario el jitter del ancho Float (el ancho del AABB es (posX+w)-posX) altera la cantidad de tiles en las plataformas móviles y las hace parpadear.
tilesToCover :: Float -> Float -> Int
tilesToCover spanLength tileSize
  | tileSize <= 0 = 1
  | otherwise = max 1 (ceiling (spanLength / tileSize - tileFitEpsilon))

tileFitEpsilon :: Float
tileFitEpsilon = 1e-3
