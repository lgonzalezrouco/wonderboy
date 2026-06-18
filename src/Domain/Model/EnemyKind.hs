{- | Catálogo de clases de enemigo (stats y tamaño de la caja de colisión).

El arquetipo de comportamiento por defecto vive en @Domain.Logic.EntityBehaviours@.
-}
module Domain.Model.EnemyKind (
  EnemyKind (..),
  EnemyMotionStats (..),
  EnemyKindStats (..),
  enemyKindStats,
  isBossKind,
  isFlyingKind,
)
where

import GHC.Generics (Generic)

import Domain.Model.BossKindStats (
  BossBatStats (..),
  BossGolemStats (..),
  bossBatStats,
  bossGolemStats,
 )
import Domain.ValueObjects.Frames (Frames, frames)
import Domain.ValueObjects.Health (Health, health)

-- | Clase de enemigo con stats compartidos por tipo.
data EnemyKind
  = SnailKind
  | BatKind
  | GolemKind
  | BossGolemKind
  | BossBatKind
  deriving (Eq, Show, Generic)

-- | Verdadero para clases de jefe (comportamiento vía catálogo de jefes).
isBossKind :: EnemyKind -> Bool
isBossKind kind = kind `elem` [BossGolemKind, BossBatKind]

-- | Verdadero para enemigos que ignoran colisión con plataformas (vuelan).
isFlyingKind :: EnemyKind -> Bool
isFlyingKind kind = kind `elem` [BatKind, BossBatKind]

{- | Parámetros de movimiento según el arquetipo natural de la clase.

Una clase patrulla /o/ reacciona: el tipo suma hace inrepresentable mezclar
parámetros de ambos (antes coexistían en un record plano con la mitad en cero).
-}
data EnemyMotionStats
  = -- | Patrulla horizontal: velocidad (px/s) y frames de espera por tramo (Snail).
    PatrolMotion Float Frames
  | -- | FSM reactivo: chaseSpeed, returnSpeed, chaseRange, spawnRadius (Golem).
    ReactiveMotion Float Float Float Float
  | -- | FSM reactivo aéreo: persigue en horizontal; en spawn patrulla en X (Bat).
    FlyingReactiveMotion Float Float Float Float Float Frames
  deriving (Eq, Show, Generic)

-- | Parámetros fijos por clase (píxeles lógicos y px/s).
data EnemyKindStats = EnemyKindStats
  { eksWidth :: Float
  -- ^ Ancho de la caja de colisión.
  , eksHeight :: Float
  -- ^ Alto de la caja de colisión.
  , eksMaxHealth :: Health
  -- ^ Salud inicial al spawnear.
  , eksMotion :: EnemyMotionStats
  -- ^ Parámetros del arquetipo de movimiento de la clase.
  }
  deriving (Eq, Show, Generic)

-- | Stats del catálogo M13.
enemyKindStats :: EnemyKind -> EnemyKindStats
enemyKindStats kind = case kind of
  SnailKind ->
    EnemyKindStats
      { eksWidth = 24
      , eksHeight = 24
      , eksMaxHealth = health 1
      , eksMotion = PatrolMotion 30 (frames 90)
      }
  BatKind ->
    EnemyKindStats
      { eksWidth = 18
      , eksHeight = 18
      , eksMaxHealth = health 1
      , eksMotion = FlyingReactiveMotion 80 40 120 24 40 (frames 60)
      }
  GolemKind ->
    EnemyKindStats
      { eksWidth = 32
      , eksHeight = 32
      , eksMaxHealth = health 2
      , eksMotion = ReactiveMotion 25 25 100 12
      }
  BossGolemKind ->
    let s = bossGolemStats
     in EnemyKindStats
          { eksWidth = bgsWidth s
          , eksHeight = bgsHeight s
          , eksMaxHealth = bgsMaxHealth s
          , eksMotion = PatrolMotion 20 (frames 120)
          }
  BossBatKind ->
    let s = bossBatStats
     in EnemyKindStats
          { eksWidth = bbsWidth s
          , eksHeight = bbsHeight s
          , eksMaxHealth = bbsMaxHealth s
          , eksMotion = PatrolMotion 40 (frames 60)
          }
