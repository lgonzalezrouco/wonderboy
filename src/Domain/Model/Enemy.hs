{- | Modelo de un enemigo dentro del mundo del juego.

Un enemigo es una __entidad__: tiene identidad propia (puede haber varios enemigos
en la misma posición y el motor necesita distinguirlos). Su 'enemyProgram' describe
el comportamiento; el intérprete en @UseCases.InterpretBehaviour@ lo ejecuta.
-}
module Domain.Model.Enemy (
  -- * Tipo
  Enemy (..),

  -- * Construcción
  mkEnemy,
)
where

import GHC.Generics (Generic)

import Domain.Logic.EntityBehaviour (BehaviourProgram)
import Domain.ValueObjects.Position (Position)
import Domain.ValueObjects.Velocity (Velocity, velocity)

{- | Estado de un enemigo en un frame dado.

__Por qué `enemyId :: Int`?__

Dos value objects con los mismos valores son indistinguibles: dos 'Position' iguales
son el mismo punto. Pero dos enemigos en la misma posición son enemigos distintos —
tienen identidad separada. El 'enemyId' es esa identidad.

Los intérpretes de comportamiento usan este id para colisiones futuras. También
facilita ignorar la colisión de una entidad consigo misma.
-}
data Enemy = Enemy
  { enemyId :: Int
  -- ^ Identificador único del enemigo en el nivel. Asignado en la carga del nivel.
  , enemyPos :: Position
  -- ^ Posición actual del enemigo en el espacio del juego (píxeles lógicos).
  , enemyVel :: Velocity
  -- ^ Velocidad actual (px/s). La fija el intérprete del DSL antes de integrar.
  , enemyProgram :: BehaviourProgram
  -- ^ Programa de comportamiento (descripción, no ejecución).
  }
  deriving (Show, Generic)

{- | Igualdad por __estado observable__: identidad, posición y velocidad.

No se compara 'enemyProgram'. El programa de comportamiento es el controlador
interno de la entidad: una descripción posiblemente cíclica (la patrulla se
construye con @fix@). Dos enemigos son iguales cuando denotan el __mismo estado
observable en el frame__, no la misma continuación de comportamiento; incluir el
programa acoplaría '==' a los internos del intérprete y, como 'BehaviourProgram'
no admite una igualdad estructural total, rompería las leyes de 'Eq'. Los tests
que necesitan inspeccionar el programa usan observadores explícitos
(@waitFramesRemaining@).
-}
instance Eq Enemy where
  a == b =
    enemyId a == enemyId b
      && enemyPos a == enemyPos b
      && enemyVel a == enemyVel b

-- | Crea un enemigo con identificador, posición y programa de comportamiento.
mkEnemy :: Int -> Position -> BehaviourProgram -> Enemy
mkEnemy eid pos prog =
  Enemy
    { enemyId = eid
    , enemyPos = pos
    , enemyVel = velocity 0 0
    , enemyProgram = prog
    }
