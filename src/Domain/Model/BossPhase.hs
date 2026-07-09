module Domain.Model.BossPhase (
  BossPhaseIndex,
  bossPhaseIndex,
  bossPhaseNumber,
  BossEventKind (..),
  BossPhaseCondition (..),
  BossPhaseDef (..),
  BossDefinition (..),
)
where

import GHC.Generics (Generic)

import Domain.Model.EntityBehaviour (BehaviourProgram)
import Domain.ValueObjects.Health (Health)
import Domain.ValueObjects.HealthRatio (HealthRatio)

-- | Qué fase de boss está activa. El índice 0 es la fase en la que aparece un boss.
newtype BossPhaseIndex = BossPhaseIndex Int
  deriving (Eq, Ord, Show, Generic)

bossPhaseIndex :: Int -> BossPhaseIndex
bossPhaseIndex n = BossPhaseIndex (max 0 n)

bossPhaseNumber :: BossPhaseIndex -> Int
bossPhaseNumber (BossPhaseIndex n) = n

data BossEventKind
  = PlayerInMeleeRange
  | TookDamageThisFrame
  deriving (Eq, Show, Generic)

data BossPhaseCondition
  = HealthAtOrBelowRatio HealthRatio
  | OnBossEvent BossEventKind
  deriving (Eq, Show, Generic)

data BossPhaseDef = BossPhaseDef
  { phaseConditions :: [BossPhaseCondition]
  -- ^ Todas las condiciones deben cumplirse para entrar a esta fase. Vacío para la fase de spawn.
  , phaseProgram :: BehaviourProgram
  }
  deriving (Show, Generic)

data BossDefinition = BossDefinition
  { bossMaxHealth :: Health
  , bossWidth :: Float
  , bossHeight :: Float
  , bossPhases :: [BossPhaseDef]
  -- ^ Fases en orden, al menos una (el índice 0 es la fase de spawn).
  }
  deriving (Show, Generic)
