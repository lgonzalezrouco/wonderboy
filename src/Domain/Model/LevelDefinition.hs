{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Esquema de definición de nivel (contenido autoral, no estado de runtime).

Tipos alineados con @levels/*.json@; la construcción del 'World' vive en
@Domain.Logic.BuildWorld@. La serialización JSON (codec DTO) vive en
@UseCases.Serialization.LevelCodec@, no aquí.
-}
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

-- | Error de validación o construcción al pasar de definición a mundo.
newtype LevelBuildError = LevelBuildError Text
  deriving (Eq, Show)

-- | Mensaje de error de carga de nivel.
levelBuildError :: Text -> LevelBuildError
levelBuildError = LevelBuildError

-- | Arquetipo de comportamiento explícito en JSON (@behaviourPreset@).
data BehaviourArchetype
  = PatrolArchetype
  | ChaseArchetype
  | GuardArchetype
  deriving (Eq, Show, Generic)

-- | Arquetipo + tuning producidos por el behaviour resolver.
data ResolvedBehaviour = ResolvedBehaviour
  { rbArchetype :: BehaviourArchetype
  , rbTuning :: BehaviourTuning
  }
  deriving (Eq, Show, Generic)

-- | Rectángulo con ancla bottom-left (plataformas y salida).
data RectDef = RectDef
  { rectPos :: Position
  , rectWidth :: Float
  , rectHeight :: Float
  }
  deriving (Eq, Show, Generic)

-- | Plataforma estática en el JSON del nivel.
newtype PlatformDef = PlatformDef {unPlatformDef :: RectDef}
  deriving (Eq, Show, Generic)

-- | Plataforma móvil en el JSON del nivel.
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

-- | Plataforma que se desmorona en el JSON del nivel.
data CrumblingPlatformDef = CrumblingPlatformDef
  { cpDefId :: Int
  , cpDefPos :: Position
  , cpDefWidth :: Float
  , cpDefHeight :: Float
  }
  deriving (Eq, Show, Generic)

-- | Colocación de enemigo en el JSON del nivel.
data EnemyDef = EnemyDef
  { enemyDefId :: Int
  , enemyDefKind :: EnemyKind
  , enemyDefPos :: Position
  , enemyDefBehaviourPreset :: Maybe BehaviourArchetype
  , enemyDefBehaviourHint :: Maybe Text
  , enemyDefBehaviourTuning :: Maybe BehaviourTuning
  }
  deriving (Eq, Show, Generic)

-- | Colocación de pickup en el JSON del nivel.
data PickupDef = PickupDef
  { pickupDefId :: Int
  , pickupDefPos :: Position
  , pickupDefValue :: Int
  }
  deriving (Eq, Show, Generic)

-- | Peligro ambiental que cae, en el JSON del nivel.
data FallingHazardDef = FallingHazardDef
  { fhDefId :: Int
  , fhDefPos :: Position
  , fhDefWidth :: Float
  , fhDefHeight :: Float
  , fhDefFallSpeed :: Float
  , fhDefLoopDelay :: Maybe Int
  }
  deriving (Eq, Show, Generic)

-- | Definición completa de un nivel.
data LevelDefinition = LevelDefinition
  { levelMinScore :: Int
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

-- | Parsea clase de enemigo desde texto de nivel.
parseEnemyKind :: Text -> Either LevelBuildError EnemyKind
parseEnemyKind txt = case txt of
  "snail" -> Right SnailKind
  "bat" -> Right BatKind
  "golem" -> Right GolemKind
  "archer" -> Right ArcherKind
  "bossGolem" -> Right BossGolemKind
  "bossBat" -> Right BossBatKind
  _ -> Left (levelBuildError ("unknown enemy kind: " <> txt))

-- | Parsea arquetipo desde @behaviourPreset@.
parseBehaviourArchetype :: Text -> Either LevelBuildError BehaviourArchetype
parseBehaviourArchetype txt = case txt of
  "patrol" -> Right PatrolArchetype
  "chase" -> Right ChaseArchetype
  "guard" -> Right GuardArchetype
  _ -> Left (levelBuildError ("unknown behaviour preset: " <> txt))
