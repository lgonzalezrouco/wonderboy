module Domain.ValueObjects.Tolerance (
  epsilon,
  near,
  nearZero,
)
where

-- | Tolerancia compartida para comparar floats en px lógicos, usada en colisión, snapping de plataformas y chequeos de extensión.
epsilon :: Float
epsilon = 1e-3

near :: Float -> Float -> Bool
near x y = abs (x - y) <= epsilon

nearZero :: Float -> Bool
nearZero x = near x 0
