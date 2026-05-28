{-# LANGUAGE DerivingStrategies #-}

-- GHC2021 habilita `GeneralisedNewtypeDeriving` pero NO la extensión
-- `DerivingStrategies`, que es la que permite escribir `deriving stock`
-- y `deriving newtype` explícitamente. La necesitamos porque la opción
-- `-Wmissing-deriving-strategies` (activada en el `warnings` del .cabal)
-- emite un warning si no especificás la estrategia en cada `deriving`.

{- | Coordenada 2D en el espacio del juego.

Representa un punto (x, y) en píxeles lógicos.
Es un Value Object: sin identidad propia, igual a otro si sus coordenadas son iguales.
-}
module Domain.ValueObjects.Position (
  Position (..),
  position,
  posX,
  posY,
)
where

import GHC.Generics (Generic)

-- `Generic` viene de `GHC.Generics` (parte de `base`).
-- Lo derivamos por completitud; en milestones posteriores Aeson lo usa para
-- serializar/deserializar a JSON sin necesidad de escribir instancias a mano.

{- | Par de coordenadas (x, y) en píxeles lógicos.

Usamos `newtype` en lugar de `data` por dos razones:

  1. __Costo cero en runtime__: un `newtype` es idéntico a su tipo interno
     en memoria; GHC lo elimina durante la compilación.
  2. __Seguridad de tipos__: aunque `Position` y `Velocity` contienen ambas un
     `(Float, Float)`, son tipos distintos para el compilador —
     pasar una `Velocity` donde se espera una `Position` es un error de compilación.

`deriving stock` le dice a GHC que use su derivación estructural incorporada
(en contraposición a `deriving newtype`, que coercionaría las instancias del
tipo interno, o `deriving anyclass`, que usaría una typeclass con Default).
Para `Eq`, `Show` y `Generic` en un `newtype`, `stock` y `newtype` producen
el mismo resultado, pero especificarlo evita ambigüedad y silencia el warning.
-}
newtype Position = Position (Float, Float)
  deriving stock (Eq, Show, Generic)

-- `Eq`      → permite comparar posiciones con `==` y `/=`.
-- `Show`    → permite imprimir posiciones en GHCi o en tests.
-- `Generic` → metadata de estructura usable por librerías (Aeson, etc.).

{- | Construye una 'Position' a partir de sus componentes x e y.

Es un /smart constructor/: en lugar de escribir `Position (x, y)` en el
código cliente, se llama a `position x y`, que es más legible y desacopla
al cliente de la representación interna (la tupla). Si en el futuro
`Position` cambiara a `Position Float Float`, sólo cambia este constructor.

En este caso no hay invariantes que validar (cualquier Float es una posición
válida), por eso el constructor de datos `Position (..)` también se exporta.
-}
position :: Float -> Float -> Position
position x y = Position (x, y)

{- | Componente horizontal de la posición.

Usa pattern matching para desestructurar el `newtype` y luego la tupla.
Preferimos un accessor nombrado sobre `fst` porque:

  * El nombre `posX` es autoexplicativo en el sitio de llamada.
  * Si la representación interna cambia, sólo se actualiza este archivo.
-}
posX :: Position -> Float
posX (Position (x, _)) = x

-- El patrón `(Position (x, _))` funciona en dos pasos:
--   1. `Position (...)` desenvuelve el newtype → obtenemos la tupla `(Float, Float)`.
--   2. `(x, _)` desenvuelve la tupla → `x` queda ligado al primer elemento,
--      `_` descarta el segundo (el compilador no genera código para él).

-- | Componente vertical de la posición.
posY :: Position -> Float
posY (Position (_, y)) = y
