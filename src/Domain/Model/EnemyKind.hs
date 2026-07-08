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

data EnemyKind
  = SnailKind
  | BatKind
  | GolemKind
  | ArcherKind
  | BossGolemKind
  | BossBatKind
  deriving (Eq, Show, Generic)

isBossKind :: EnemyKind -> Bool
isBossKind kind = kind `elem` [BossGolemKind, BossBatKind]

isFlyingKind :: EnemyKind -> Bool
isFlyingKind kind = kind `elem` [BatKind, BossBatKind]

data EnemyMotionStats
  = PatrolMotion Float Frames -- velocidad px/s, frames de espera por tramo (Snail)
  | ReactiveMotion Float Float Float Float -- chaseSpeed, returnSpeed, chaseRange, spawnRadius (Golem)
  | FlyingReactiveMotion Float Float Float Float Float Frames -- chaseSpeed, returnSpeed, chaseRange, spawnRadius, patrolSpeed, tramo de patrulla (Bat)
  | ArcherMotion Float Frames Float Frames Float Float -- shootRange, cooldown, velocidad, lifetime, ancho, alto del proyectil (Archer)
  deriving (Eq, Show, Generic)

data EnemyKindStats = EnemyKindStats
  { eksWidth :: Float
  -- ^ Tamaño de la caja de colisión en px lógicos (va con eksHeight).
  , eksHeight :: Float
  , eksMaxHealth :: Health
  , eksMotion :: EnemyMotionStats
  }
  deriving (Eq, Show, Generic)

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
  ArcherKind ->
    EnemyKindStats
      { eksWidth = 24
      , eksHeight = 24
      , eksMaxHealth = health 1
      , eksMotion = ArcherMotion 160 (frames 90) 200 (frames 120) 8 8
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
