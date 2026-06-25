{- | Lógica pura de teselado: cuántos tiles cubren un ancho dado.

Separada del armado de 'Picture' (como 'Adapters.Gloss.Camera') por ser la única
parte numéricamente delicada del render de plataformas.
-}
module Adapters.Gloss.Tiling (
  tilesToCover,
)
where

{- | Cantidad de tiles de 'tileSize' que cubren 'spanLength'.

El descuento de 'tileFitEpsilon' antes del 'ceiling' es lo que evita el parpadeo
de las plataformas móviles: el render deriva el ancho del AABB como
@(posX + w) - posX@, inexacto en 'Float', así que cuando el cociente cae justo
sobre un entero (105 px sobre tiles de 35: 105 / 35 = 3) el 'ceiling' salta entre
3 y 4 a medida que cambia 'posX'. La tolerancia ancla el conteo al múltiplo y solo
suma un tile cuando el ancho lo supera de forma perceptible.
-}
tilesToCover :: Float -> Float -> Int
tilesToCover spanLength tileSize
  | tileSize <= 0 = 1
  | otherwise = max 1 (ceiling (spanLength / tileSize - tileFitEpsilon))

-- | Holgura (en tiles) muy por encima del ruido de 'Float' y muy por debajo de 1.
tileFitEpsilon :: Float
tileFitEpsilon = 1e-3
