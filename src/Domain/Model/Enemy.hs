{- | Modelo de un enemigo dentro del mundo del juego.

Un enemigo es una __entidad__: tiene identidad propia (puede haber varios enemigos
en la misma posición y el motor necesita distinguirlos). Su comportamiento se describe
mediante el DSL de entidades (Free monad, Milestone 6); aquí sólo modelamos el estado.
-}
module Domain.Model.Enemy
  ( -- * Tipo
    Enemy (..)
  , -- * Construcción
    mkEnemy
  )
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Position (Position)
import Domain.ValueObjects.Velocity (Velocity, velocity)

{- | Estado de un enemigo en un frame dado.

__Por qué `enemyId :: Int`?__

Dos value objects con los mismos valores son indistinguibles: dos 'Position' iguales
son el mismo punto. Pero dos enemigos en la misma posición son enemigos distintos —
tienen identidad separada. El 'enemyId' es esa identidad.

En el DSL (M6), los intérpretes de comportamiento usarán este id para identificar
sobre qué enemigo actúa una instrucción. También facilita la detección de colisiones
(M3): se puede ignorar la colisión de una entidad consigo misma.

No usamos `newtype EnemyId = EnemyId Int` por simplicidad en M2; si el tamaño del
proyecto lo justifica, puede crearse el newtype en M3+ para mayor seguridad de tipos.
-}
data Enemy = Enemy
  { enemyId :: Int
  -- ^ Identificador único del enemigo en el nivel. Asignado en la carga del nivel.
  , enemyPos :: Position
  -- ^ Posición actual del enemigo en el espacio del juego (píxeles lógicos).
  , enemyVel :: Velocity
  -- ^ Velocidad actual (px/s). Actualizada por el intérprete de comportamiento (M6).
  }
  deriving (Eq, Show, Generic)

{- | Crea un enemigo con el identificador y posición dados, en reposo.

El comportamiento inicial (patrullar, perseguir, etc.) lo asigna el intérprete
del DSL de entidades (Milestone 6). En M2, los enemigos arrancan quietos.
-}
mkEnemy :: Int -> Position -> Enemy
mkEnemy eid pos =
  Enemy
    { enemyId = eid
    , enemyPos = pos
    , enemyVel = velocity 0 0 -- en reposo hasta que el DSL lo mueva (M6)
    }
