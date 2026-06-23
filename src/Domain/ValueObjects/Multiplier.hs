{- | Factor de escala validado para perillas de gameplay (velocidad, alcance, salud).

Value object con invariante: el factor siempre vive en @[minMultiplier, maxMultiplier]@.
El constructor de datos NO se exporta; se fuerza el smart constructor 'mkMultiplier', que
clampea. Así ninguna salida del modelo (por más rara que sea) puede romper el balance.
-}
module Domain.ValueObjects.Multiplier (
  Multiplier,
  mkMultiplier,
  identityMultiplier,
  unMultiplier,
)
where

import GHC.Generics (Generic)

-- | Factor de escala (1.0 = sin cambio). Opaco: construir solo vía 'mkMultiplier'.
newtype Multiplier = Multiplier Float
  deriving (Eq, Ord, Show, Generic)

-- | Cotas del factor. Fuera de esto un enemigo se vuelve injugable o trivial.
minMultiplier, maxMultiplier :: Float
minMultiplier = 0.3
maxMultiplier = 3.0

-- | Factor neutro (1.0): no cambia nada.
identityMultiplier :: Multiplier
identityMultiplier = Multiplier 1.0

{- | Construye un 'Multiplier' clampeando a @[minMultiplier, maxMultiplier]@.

Los valores no finitos ('NaN', '±∞' — posibles si el modelo devuelve basura) caen a
'identityMultiplier': las comparaciones con 'NaN' son 'False', así que un clamp con
@min@/@max@ no alcanzaría y hay que descartarlos explícitamente.
-}
mkMultiplier :: Float -> Multiplier
mkMultiplier x
  | isNaN x || isInfinite x = identityMultiplier
  | otherwise = Multiplier (max minMultiplier (min maxMultiplier x))

-- | Extrae el factor como 'Float'.
unMultiplier :: Multiplier -> Float
unMultiplier (Multiplier x) = x
