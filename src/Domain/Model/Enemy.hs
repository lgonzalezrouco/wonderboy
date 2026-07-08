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
import Domain.ValueObjects.Frames (Frames, frames)
import Domain.ValueObjects.Health (Health)
import Domain.ValueObjects.Position (Position)
import Domain.ValueObjects.Velocity (Velocity, velocity)

-- | Estado de un enemigo en un frame dado.
data Enemy = Enemy
  { enemyId :: Int
  , enemyKind :: EnemyKind
  , enemyPos :: Position
  , enemyVel :: Velocity
  , enemyHealth :: Health
  , enemyMaxHealth :: Health
  , enemySpawnPos :: Position
  -- ^ Spawn anchor para FSM de retorno.
  , enemyFacing :: Facing
  , enemyProgram :: BehaviourProgram
  , enemyBossPhase :: Maybe BossPhaseIndex
  -- ^ Fase actual del jefe; 'Nothing' para enemigos regulares.
  , enemyShootCooldownFrames :: Frames
  -- ^ Frames restantes antes de poder disparar de nuevo (Archer).
  , enemyHurtFrames :: Frames
  -- ^ Destello visual tras daño del jugador que no derrota al enemigo.
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

enemyWidth :: Enemy -> Float
enemyWidth = eksWidth . enemyStats

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
        , enemyShootCooldownFrames = frames 0
        , enemyHurtFrames = frames 0
        }

mkEnemy :: Int -> Position -> BehaviourProgram -> Enemy
mkEnemy eid = spawnEnemy eid SnailKind
