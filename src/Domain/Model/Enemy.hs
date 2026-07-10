module Domain.Model.Enemy (
  Enemy (..),
  enemyWidth,
  enemyHeight,
  enemyAabb,
  enemyInPhaseTransition,
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
import Domain.ValueObjects.Frames (Frames, frames, hasFramesLeft, noFrames)
import Domain.ValueObjects.Health (Health)
import Domain.ValueObjects.Position (Position)
import Domain.ValueObjects.Velocity (Velocity, velocity)

data Enemy = Enemy
  { enemyId :: Int
  , enemyKind :: EnemyKind
  , enemyPos :: Position
  , enemyVel :: Velocity
  , enemyHealth :: Health
  , enemyMaxHealth :: Health
  , enemySpawnPos :: Position
  -- ^ Ancla a la que vuelve un enemigo reactivo (FSMs de chase/guard).
  , enemyFacing :: Facing
  , enemyProgram :: BehaviourProgram
  , enemyBossPhase :: Maybe BossPhaseIndex
  , enemyShootCooldownFrames :: Frames
  , enemyHurtFrames :: Frames
  -- ^ Temporizador del destello de golpe cuando el jugador daña al enemigo pero no lo mata.
  , enemyPhaseTransition :: Frames
  -- ^ Pausa tras cambiar de fase: mientras corre, el jefe está congelado e invulnerable.
  }
  deriving (Show, Generic)

-- Eq a mano porque enemyProgram no tiene Eq. Compara solo el estado
-- observable (id, kind, pos, vel, salud, facing, fase de boss).
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

enemyAabb :: Enemy -> Aabb
enemyAabb e =
  aabbFromBottomCenter (enemyPos e) (enemyWidth e) (enemyHeight e)

enemyInPhaseTransition :: Enemy -> Bool
enemyInPhaseTransition = hasFramesLeft . enemyPhaseTransition

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
        , enemyPhaseTransition = noFrames
        }

mkEnemy :: Int -> Position -> BehaviourProgram -> Enemy
mkEnemy eid = spawnEnemy eid SnailKind
