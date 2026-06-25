{- | Factor de amplificación validado para perillas que solo potencian (alcance, salud).

Value object con invariante: el factor siempre vive en @[minAmplifier, maxAmplifier]@, con
piso 1.0. A diferencia de 'Domain.ValueObjects.Multiplier' (que puede reducir, hasta 0.3),
un 'Amplifier' nunca baja del valor base: modela perillas donde "menos que la base" no
tiene sentido jugable —un enemigo de 1 HP no puede ser más frágil, y un rango de detección
ya corto no debe achicarse hasta obligar a estar pegado—. El constructor de datos NO se
exporta; se fuerza el smart constructor 'mkAmplifier', que clampea.
-}
module Domain.ValueObjects.Amplifier (
  Amplifier,
  mkAmplifier,
  identityAmplifier,
  unAmplifier,
)
where

import GHC.Generics (Generic)

-- | Factor de amplificación (1.0 = base, sin amplificar). Opaco: construir vía 'mkAmplifier'.
newtype Amplifier = Amplifier Float
  deriving (Eq, Ord, Show, Generic)

-- | Cotas del factor. El piso 1.0 es lo que hace que un 'Amplifier' solo potencie, jamás reduzca.
minAmplifier, maxAmplifier :: Float
minAmplifier = 1.0
maxAmplifier = 3.0

-- | Factor neutro (1.0): deja el valor base del arquetipo intacto.
identityAmplifier :: Amplifier
identityAmplifier = Amplifier 1.0

{- | Construye un 'Amplifier' clampeando a @[minAmplifier, maxAmplifier]@.

Los valores no finitos ('NaN', '±∞' — posibles si el modelo devuelve basura) caen a
'identityAmplifier': las comparaciones con 'NaN' son 'False', así que un clamp con
@min@/@max@ no alcanzaría y hay que descartarlos explícitamente.
-}
mkAmplifier :: Float -> Amplifier
mkAmplifier x
  | isNaN x || isInfinite x = identityAmplifier
  | otherwise = Amplifier (max minAmplifier (min maxAmplifier x))

-- | Extrae el factor como 'Float'.
unAmplifier :: Amplifier -> Float
unAmplifier (Amplifier x) = x
