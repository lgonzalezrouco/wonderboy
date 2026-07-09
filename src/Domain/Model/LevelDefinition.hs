{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Model.LevelDefinition (
  BehaviourArchetype (..),
  BossArenaDef (..),
  CrumblingPlatformDef (..),
  EnemyDef (..),
  FallingHazardDef (..),
  LevelBuildError (..),
  LevelDefinition (..),
  MovingPlatformDef (..),
  PickupDef (..),
  PlatformDef (..),
  RectDef (..),
  ResolvedBehaviour (..),
  levelBuildError,
  parseBehaviourArchetype,
  parseEnemyKind,
)
where

import Data.Text (Text)
import GHC.Generics (Generic)

import Domain.Model.BossArena (BossArenaDef (..))
import Domain.Model.EnemyKind (EnemyKind (..))
import Domain.ValueObjects.BehaviourTuning (BehaviourTuning)
import Domain.ValueObjects.Position (Position)

newtype LevelBuildError = LevelBuildError Text
  deriving (Eq, Show)

levelBuildError :: Text -> LevelBuildError
levelBuildError = LevelBuildError

data BehaviourArchetype
  = PatrolArchetype
  | ChaseArchetype
  | GuardArchetype
  deriving (Eq, Show, Generic)

data ResolvedBehaviour = ResolvedBehaviour
  { rbArchetype :: BehaviourArchetype
  , rbTuning :: BehaviourTuning
  }
  deriving (Eq, Show, Generic)

data RectDef = RectDef
  { rectPos :: Position
  , rectWidth :: Float
  , rectHeight :: Float
  }
  deriving (Eq, Show, Generic)

newtype PlatformDef = PlatformDef {unPlatformDef :: RectDef}
  deriving (Eq, Show, Generic)

data MovingPlatformDef = MovingPlatformDef
  { mpDefId :: Int
  , mpDefPos :: Position
  , mpDefWidth :: Float
  , mpDefHeight :: Float
  , mpDefEndA :: Position
  , mpDefEndB :: Position
  , mpDefSpeed :: Float
  , mpDefStartTowardB :: Bool
  }
  deriving (Eq, Show, Generic)

data CrumblingPlatformDef = CrumblingPlatformDef
  { cpDefId :: Int
  , cpDefPos :: Position
  , cpDefWidth :: Float
  , cpDefHeight :: Float
  }
  deriving (Eq, Show, Generic)

data EnemyDef = EnemyDef
  { enemyDefId :: Int
  , enemyDefKind :: EnemyKind
  , enemyDefPos :: Position
  , enemyDefBehaviourPreset :: Maybe BehaviourArchetype
  -- ^ Arquetipo explícito del archivo de nivel, si lo hay (saltea la resolución del hint).
  , enemyDefBehaviourHint :: Maybe Text
  -- ^ Descripción de comportamiento en texto libre, que se resuelve a un preset cuando no se da uno.
  , enemyDefBehaviourTuning :: Maybe BehaviourTuning
  -- ^ Tuning numérico opcional que se aplica sobre el arquetipo.
  }
  deriving (Eq, Show, Generic)

data PickupDef = PickupDef
  { pickupDefId :: Int
  , pickupDefPos :: Position
  , pickupDefValue :: Int
  }
  deriving (Eq, Show, Generic)

data FallingHazardDef = FallingHazardDef
  { fhDefId :: Int
  , fhDefPos :: Position
  , fhDefWidth :: Float
  , fhDefHeight :: Float
  , fhDefFallSpeed :: Float
  , fhDefLoopDelay :: Maybe Int
  -- ^ Frames antes de que el hazard vuelva a caer. Nothing significa que cae una sola vez.
  }
  deriving (Eq, Show, Generic)

data LevelDefinition = LevelDefinition
  { levelMinScore :: Int
  -- ^ Puntaje que el jugador debe alcanzar antes de que la salida complete el nivel.
  , levelSpawn :: Position
  , levelPlatforms :: [PlatformDef]
  , levelMovingPlatforms :: [MovingPlatformDef]
  , levelEnemies :: [EnemyDef]
  , levelPickups :: [PickupDef]
  , levelFallingHazards :: [FallingHazardDef]
  , levelCrumblingPlatforms :: [CrumblingPlatformDef]
  , levelBossArena :: Maybe BossArenaDef
  , levelExit :: RectDef
  }
  deriving (Eq, Show, Generic)

parseEnemyKind :: Text -> Either LevelBuildError EnemyKind
parseEnemyKind txt = case txt of
  "snail" -> Right SnailKind
  "bat" -> Right BatKind
  "golem" -> Right GolemKind
  "archer" -> Right ArcherKind
  "bossGolem" -> Right BossGolemKind
  "bossBat" -> Right BossBatKind
  _ -> Left (levelBuildError ("unknown enemy kind: " <> txt))

parseBehaviourArchetype :: Text -> Either LevelBuildError BehaviourArchetype
parseBehaviourArchetype txt = case txt of
  "patrol" -> Right PatrolArchetype
  "chase" -> Right ChaseArchetype
  "guard" -> Right GuardArchetype
  _ -> Left (levelBuildError ("unknown behaviour preset: " <> txt))
