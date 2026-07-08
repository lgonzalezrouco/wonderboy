{- | Tipos de fases de jefe y condiciones de transición (catálogo DSL).

Las fases de jefe son distintas de las fases de juego ('Domain.Model.GamePhase'):
aquí se modela un tramo del combate contra un jefe con su propio programa de
comportamiento.
-}
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

-- | Índice de fase de jefe (0 = fase inicial al spawnear).
newtype BossPhaseIndex = BossPhaseIndex Int
  deriving (Eq, Ord, Show, Generic)

-- | Construye un índice de fase de jefe (satura en 0 si es negativo).
bossPhaseIndex :: Int -> BossPhaseIndex
bossPhaseIndex n = BossPhaseIndex (max 0 n)

-- | Índice numérico de la fase (para pruebas y depuración).
bossPhaseNumber :: BossPhaseIndex -> Int
bossPhaseNumber (BossPhaseIndex n) = n

-- | Eventos observables para transiciones de fase (además de umbrales de salud).
data BossEventKind
  = -- | Jugador a distancia horizontal de melee o menos.
    PlayerInMeleeRange
  | -- | El jefe perdió salud en el paso de combate del frame actual.
    TookDamageThisFrame
  deriving (Eq, Show, Generic)

-- | Condición de entrada a una fase de jefe (todas deben cumplirse).
data BossPhaseCondition
  = -- | Salud actual / salud máxima ≤ umbral.
    HealthAtOrBelowRatio HealthRatio
  | -- | Un evento de jefe ocurrió este frame.
    OnBossEvent BossEventKind
  deriving (Eq, Show, Generic)

-- | Una fase de jefe: condiciones de entrada y programa de comportamiento.
data BossPhaseDef = BossPhaseDef
  { phaseConditions :: [BossPhaseCondition]
  -- ^ Vacío en la fase 0 (spawn).
  , phaseProgram :: BehaviourProgram
  -- ^ Programa activo mientras el jefe permanece en esta fase.
  }
  deriving (Show, Generic)

-- | Definición de catálogo para una clase de jefe.
data BossDefinition = BossDefinition
  { bossMaxHealth :: Health
  -- ^ Salud inicial al spawnear.
  , bossWidth :: Float
  , bossHeight :: Float
  , bossPhases :: [BossPhaseDef]
  -- ^ Fases ordenadas; al menos una.
  }
  deriving (Show, Generic)
