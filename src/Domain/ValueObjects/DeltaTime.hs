{- | Tiempo transcurrido entre dos frames consecutivos, en segundos.

Este value object encapsula el intervalo de tiempo que usa el motor
en cada tick de simulación. A diferencia de 'Position' y 'Velocity',
aquí sí hay un __invariante__: el tiempo no puede ser negativo.
Por eso el constructor de datos está oculto y sólo se exporta el
smart constructor 'deltaTime', que garantiza el invariante.
-}
module Domain.ValueObjects.DeltaTime (
  -- * Tipo
  DeltaTime,

  -- * Construcción
  deltaTime,

  -- * Acceso
  seconds,
)
where

import GHC.Generics (Generic)

{- | Intervalo de tiempo entre frames, en segundos.

Usamos `newtype` (no `data`) por las mismas razones que 'Position':
costo cero en runtime y distinción de tipos en compilación. Un `Float`
genérico podría ser negativo; 'DeltaTime' garantiza que no lo es.

El constructor de datos __no se exporta__: se fuerza el uso del smart
constructor 'deltaTime', que aplica el invariante de no-negatividad.
-}
newtype DeltaTime = DeltaTime Float
  deriving (Eq, Show, Generic)

-- Usamos `deriving` sin estrategia explícita (igual que `Position` y `Velocity`):
-- GHC infiere que `stock` es la estrategia correcta para `Eq`, `Show` y `Generic`
-- en un `newtype`. La extensión `DerivingStrategies` no es necesaria aquí.

{- | Construye un 'DeltaTime' garantizando que el valor sea ≥ 0.

Si el argumento es negativo (por ejemplo, por un bug del adaptador de tiempo),
se devuelve 0 en lugar de propagar el valor inválido al motor de física.

Éste es el ejemplo canónico en el proyecto de /smart constructor con invariante/:
el contrato queda en un solo lugar y el resto del código puede asumir que
`seconds dt ≥ 0` sin verificarlo en cada uso.

@
deltaTime 0.016  -- 16 ms (aprox. 60 FPS)
deltaTime (-1)   -- devuelve DeltaTime 0 (no falla, pero descarta el error)
@
-}
deltaTime :: Float -> DeltaTime
deltaTime t = DeltaTime (max 0 t)

-- `max 0 t` es la forma más directa de saturar en cero:
--   * `max :: Ord a => a -> a -> a` devuelve el mayor de dos valores.
--   * Si t >= 0, `max 0 t = t`; si t < 0, `max 0 t = 0`.

{- | Extrae el valor en segundos de un 'DeltaTime'.

Garantizado por construcción: `seconds dt >= 0`.
-}
seconds :: DeltaTime -> Float
seconds (DeltaTime t) = t
