{- | Modelo de un enemigo dentro del mundo del juego.

Un enemigo es una __entidad__: tiene identidad propia (puede haber varios enemigos
en la misma posición y el motor necesita distinguirlos). Su 'enemyProgram' describe
el comportamiento; el intérprete puro en @Domain.Logic.RunBehaviour@ lo ejecuta.
-}
module Domain.Model.Enemy (
  -- * Tipo
  Enemy (..),

  -- * Caja de colisión
  enemyWidth,
  enemyHeight,
  enemyAabb,

  -- * Construcción
  spawnEnemy,
  mkEnemy,
)
where

import GHC.Generics (Generic)

import Domain.Model.BossPhase (BossPhaseIndex, bossPhaseIndex)
import Domain.Model.EnemyKind (
  EnemyKind (..),
  EnemyKindStats (..),
  enemyKindStats,
  isBossKind,
 )
import Domain.Model.EntityBehaviour (BehaviourProgram)
import Domain.ValueObjects.Aabb (Aabb, aabbFromBottomCenter)
import Domain.ValueObjects.Facing (Facing (..))
import Domain.ValueObjects.Health (Health)
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
  , enemyKind :: EnemyKind
  -- ^ Clase de enemigo (stats y caja de colisión).
  , enemyPos :: Position
  -- ^ Posición actual del enemigo en el espacio del juego (píxeles lógicos).
  , enemyVel :: Velocity
  -- ^ Velocidad actual (px/s). La fija el intérprete del DSL antes de integrar.
  , enemyHealth :: Health
  -- ^ Salud actual; al llegar a 0 el enemigo es derrotado.
  , enemyMaxHealth :: Health
  -- ^ Salud máxima de esta instancia (umbrales de jefe y barra HUD).
  , enemySpawnPos :: Position
  -- ^ Spawn anchor para FSM de retorno.
  , enemyFacing :: Facing
  -- ^ Orientación horizontal (render y persecución).
  , enemyProgram :: BehaviourProgram
  -- ^ Programa de comportamiento (descripción, no ejecución).
  , enemyBossPhase :: Maybe BossPhaseIndex
  -- ^ Fase actual del jefe; 'Nothing' para enemigos regulares.
  }
  deriving (Show, Generic)

{- | Igualdad por __estado observable__: identidad, clase, posición, velocidad,
salud, facing y fase de jefe.

No se compara 'enemyProgram' ni 'enemySpawnPos'.
-}
instance Eq Enemy where
  a == b =
    enemyId a == enemyId b
      && enemyKind a == enemyKind b
      && enemyPos a == enemyPos b
      && enemyVel a == enemyVel b
      && enemyHealth a == enemyHealth b
      && enemyFacing a == enemyFacing b
      && enemyBossPhase a == enemyBossPhase b

enemyStats :: Enemy -> EnemyKindStats
enemyStats = enemyKindStats . enemyKind

-- | Ancho de la caja de colisión según la clase del enemigo.
enemyWidth :: Enemy -> Float
enemyWidth = eksWidth . enemyStats

-- | Alto de la caja de colisión según la clase del enemigo.
enemyHeight :: Enemy -> Float
enemyHeight = eksHeight . enemyStats

-- | Caja de colisión del enemigo: @enemyPos@ es el centro inferior (pies).
enemyAabb :: Enemy -> Aabb
enemyAabb e =
  aabbFromBottomCenter (enemyPos e) (enemyWidth e) (enemyHeight e)

-- | Crea un enemigo con clase, posición y programa (salud y spawn desde kind).
spawnEnemy :: Int -> EnemyKind -> Position -> BehaviourProgram -> Enemy
spawnEnemy eid kind pos prog =
  let stats = enemyKindStats kind
      bossPhase =
        if isBossKind kind
          then Just (bossPhaseIndex 0)
          else Nothing
   in Enemy
        { enemyId = eid
        , enemyKind = kind
        , enemyPos = pos
        , enemyVel = velocity 0 0
        , enemyHealth = eksMaxHealth stats
        , enemyMaxHealth = eksMaxHealth stats
        , enemySpawnPos = pos
        , enemyFacing = FacingRight
        , enemyProgram = prog
        , enemyBossPhase = bossPhase
        }

-- | Crea un enemigo Snail para tests con programa explícito.
mkEnemy :: Int -> Position -> BehaviourProgram -> Enemy
mkEnemy eid = spawnEnemy eid SnailKind
