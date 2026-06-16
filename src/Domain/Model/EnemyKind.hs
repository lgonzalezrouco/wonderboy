{- | Catálogo de clases de enemigo (stats y tamaño de hitbox).

El arquetipo de comportamiento por defecto vive en @Domain.Logic.EntityBehaviours@.
-}
module Domain.Model.EnemyKind (
  EnemyKind (..),
  EnemyKindStats (..),
  enemyKindStats,
)
where

import GHC.Generics (Generic)

-- | Clase de enemigo con stats compartidos por tipo.
data EnemyKind
  = SnailKind
  | BatKind
  | GolemKind
  deriving (Eq, Show, Generic)

-- | Parámetros fijos por clase (píxeles lógicos y px/s).
data EnemyKindStats = EnemyKindStats
  { eksWidth :: Float
  -- ^ Ancho del collision box.
  , eksHeight :: Float
  -- ^ Alto del collision box.
  , eksMaxHealth :: Int
  -- ^ Salud inicial al spawnear.
  , eksPatrolSpeed :: Float
  -- ^ Velocidad de patrulla horizontal (Snail).
  , eksPatrolFrames :: Int
  -- ^ Frames de espera por tramo de patrulla.
  , eksChaseSpeed :: Float
  -- ^ Velocidad al perseguir al jugador.
  , eksReturnSpeed :: Float
  -- ^ Velocidad al volver al spawn anchor.
  , eksChaseRange :: Float
  -- ^ Umbral horizontal de chase (abs Δx entre pies).
  , eksSpawnRadius :: Float
  -- ^ Radio horizontal para considerar “en spawn”.
  }
  deriving (Eq, Show, Generic)

-- | Stats del catálogo M13.
enemyKindStats :: EnemyKind -> EnemyKindStats
enemyKindStats kind = case kind of
  SnailKind ->
    EnemyKindStats
      { eksWidth = 24
      , eksHeight = 24
      , eksMaxHealth = 1
      , eksPatrolSpeed = 30
      , eksPatrolFrames = 90
      , eksChaseSpeed = 0
      , eksReturnSpeed = 0
      , eksChaseRange = 0
      , eksSpawnRadius = 0
      }
  BatKind ->
    EnemyKindStats
      { eksWidth = 18
      , eksHeight = 18
      , eksMaxHealth = 1
      , eksPatrolSpeed = 0
      , eksPatrolFrames = 0
      , eksChaseSpeed = 80
      , eksReturnSpeed = 40
      , eksChaseRange = 120
      , eksSpawnRadius = 8
      }
  GolemKind ->
    EnemyKindStats
      { eksWidth = 32
      , eksHeight = 32
      , eksMaxHealth = 2
      , eksPatrolSpeed = 0
      , eksPatrolFrames = 0
      , eksChaseSpeed = 25
      , eksReturnSpeed = 25
      , eksChaseRange = 100
      , eksSpawnRadius = 12
      }
