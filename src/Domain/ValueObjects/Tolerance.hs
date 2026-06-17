{- | Tolerancia de igualdad aproximada para comparaciones en punto flotante.

Una única tolerancia compartida por la colisión AABB, el snapping de plataformas
móviles al extremo del recorrido y la validación de extremos sobre un eje. Tener
un solo 'epsilon' evita que, al re-tunear la tolerancia en una parte, las otras
queden con el valor viejo y la geometría derive.
-}
module Domain.ValueObjects.Tolerance (
  epsilon,
  near,
  nearZero,
)
where

-- | Tolerancia compartida en píxeles lógicos para comparaciones de geometría.
epsilon :: Float
epsilon = 1e-3

-- | 'True' si dos valores difieren en a lo sumo 'epsilon'.
near :: Float -> Float -> Bool
near x y = abs (x - y) <= epsilon

-- | 'True' si un valor está a lo sumo 'epsilon' de cero.
nearZero :: Float -> Bool
nearZero x = near x 0
