{-# LANGUAGE OverloadedStrings #-}

module UseCases.Serialization.LevelCodec (
  decodeLevelText,
  encodeLevelDefinitionText,
  LevelDefinitionDTO (..),
  RectDTO (..),
  PlatformDefDTO (..),
  MovingPlatformDefDTO (..),
  CrumblingPlatformDefDTO (..),
  EnemyDefDTO (..),
  PickupDefDTO (..),
  FallingHazardDefDTO (..),
  BossArenaDefDTO (..),
  PositionDTO (..),
  levelDefinitionFromDTO,
  levelDefinitionToDTO,
  positionFromDTO,
  positionToDTO,
)
where

import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)

import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  eitherDecodeStrict,
  encode,
  object,
  withObject,
  (.:),
  (.:?),
  (.=),
 )
import Data.Aeson.Key (Key)
import Data.Aeson.Types (Pair)
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Domain.Model.BossArena (BossArenaDef (..))
import Domain.Model.EnemyKind (EnemyKind (..))
import Domain.Model.LevelDefinition (
  BehaviourArchetype (..),
  CrumblingPlatformDef (..),
  EnemyDef (..),
  FallingHazardDef (..),
  LevelBuildError (..),
  LevelDefinition (..),
  MovingPlatformDef (..),
  PickupDef (..),
  PlatformDef (..),
  RectDef (..),
  parseBehaviourArchetype,
  parseEnemyKind,
 )
import Domain.ValueObjects.Position (Position, posX, posY, position)

data PositionDTO = PositionDTO
  { dtoPosX :: Float
  , dtoPosY :: Float
  }
  deriving (Eq, Show, Generic)

data RectDTO = RectDTO
  { dtoRectPos :: PositionDTO
  , dtoRectWidth :: Float
  , dtoRectHeight :: Float
  }
  deriving (Eq, Show, Generic)

newtype PlatformDefDTO = PlatformDefDTO {unPlatformDefDTO :: RectDTO}
  deriving (Eq, Show, Generic)

data MovingPlatformDefDTO = MovingPlatformDefDTO
  { dtoMpId :: Int
  , dtoMpPos :: PositionDTO
  , dtoMpWidth :: Float
  , dtoMpHeight :: Float
  , dtoMpEndA :: PositionDTO
  , dtoMpEndB :: PositionDTO
  , dtoMpSpeed :: Float
  , dtoMpStartTowardB :: Bool
  }
  deriving (Eq, Show, Generic)

data CrumblingPlatformDefDTO = CrumblingPlatformDefDTO
  { dtoCpId :: Int
  , dtoCpPos :: PositionDTO
  , dtoCpWidth :: Float
  , dtoCpHeight :: Float
  }
  deriving (Eq, Show, Generic)

data EnemyDefDTO = EnemyDefDTO
  { dtoEnemyId :: Int
  , dtoEnemyKindText :: Text
  , dtoEnemyPos :: PositionDTO
  , dtoEnemyPreset :: Maybe Text
  , dtoEnemyHint :: Maybe Text
  }
  deriving (Eq, Show, Generic)

data PickupDefDTO = PickupDefDTO
  { dtoPickupId :: Int
  , dtoPickupPos :: PositionDTO
  , dtoPickupValue :: Int
  }
  deriving (Eq, Show, Generic)

data FallingHazardDefDTO = FallingHazardDefDTO
  { dtoFhId :: Int
  , dtoFhPos :: PositionDTO
  , dtoFhWidth :: Float
  , dtoFhHeight :: Float
  , dtoFhFallSpeed :: Float
  , dtoFhLoopDelay :: Maybe Int
  }
  deriving (Eq, Show, Generic)

data BossArenaDefDTO = BossArenaDefDTO
  { dtoBossLeft :: Float
  , dtoBossRight :: Float
  }
  deriving (Eq, Show, Generic)

data LevelDefinitionDTO = LevelDefinitionDTO
  { dtoMinScore :: Int
  , dtoSpawn :: PositionDTO
  , dtoPlatforms :: [PlatformDefDTO]
  , dtoMovingPlatforms :: [MovingPlatformDefDTO]
  , dtoEnemies :: [EnemyDefDTO]
  , dtoPickups :: [PickupDefDTO]
  , dtoFallingHazards :: [FallingHazardDefDTO]
  , dtoCrumblingPlatforms :: [CrumblingPlatformDefDTO]
  , dtoBossArena :: Maybe BossArenaDefDTO
  , dtoExit :: RectDTO
  }
  deriving (Eq, Show, Generic)

optField :: (ToJSON v) => Key -> Maybe v -> [Pair]
optField k = maybe [] (\v -> [k .= v])

instance FromJSON PositionDTO where
  parseJSON = withObject "Position" $ \o ->
    PositionDTO <$> o .: "x" <*> o .: "y"

instance ToJSON PositionDTO where
  toJSON p = object ["x" .= dtoPosX p, "y" .= dtoPosY p]

instance FromJSON RectDTO where
  parseJSON = withObject "Rect" $ \o ->
    RectDTO <$> o .: "pos" <*> o .: "width" <*> o .: "height"

instance ToJSON RectDTO where
  toJSON r =
    object
      [ "pos" .= dtoRectPos r
      , "width" .= dtoRectWidth r
      , "height" .= dtoRectHeight r
      ]

instance FromJSON PlatformDefDTO where
  parseJSON v = PlatformDefDTO <$> parseJSON v

instance ToJSON PlatformDefDTO where
  toJSON (PlatformDefDTO r) = toJSON r

instance FromJSON MovingPlatformDefDTO where
  parseJSON = withObject "MovingPlatform" $ \o ->
    MovingPlatformDefDTO
      <$> o .: "id"
      <*> o .: "pos"
      <*> o .: "width"
      <*> o .: "height"
      <*> o .: "endA"
      <*> o .: "endB"
      <*> o .: "speed"
      <*> o .: "startTowardB"

instance ToJSON MovingPlatformDefDTO where
  toJSON mp =
    object
      [ "id" .= dtoMpId mp
      , "pos" .= dtoMpPos mp
      , "width" .= dtoMpWidth mp
      , "height" .= dtoMpHeight mp
      , "endA" .= dtoMpEndA mp
      , "endB" .= dtoMpEndB mp
      , "speed" .= dtoMpSpeed mp
      , "startTowardB" .= dtoMpStartTowardB mp
      ]

instance FromJSON CrumblingPlatformDefDTO where
  parseJSON = withObject "CrumblingPlatform" $ \o ->
    CrumblingPlatformDefDTO
      <$> o .: "id"
      <*> o .: "pos"
      <*> o .: "width"
      <*> o .: "height"

instance ToJSON CrumblingPlatformDefDTO where
  toJSON cp =
    object
      [ "id" .= dtoCpId cp
      , "pos" .= dtoCpPos cp
      , "width" .= dtoCpWidth cp
      , "height" .= dtoCpHeight cp
      ]

instance FromJSON EnemyDefDTO where
  parseJSON = withObject "Enemy" $ \o ->
    EnemyDefDTO
      <$> o .: "id"
      <*> o .: "kind"
      <*> o .: "pos"
      <*> o .:? "behaviourPreset"
      <*> o .:? "behaviourHint"

instance ToJSON EnemyDefDTO where
  toJSON e =
    object $
      [ "id" .= dtoEnemyId e
      , "kind" .= dtoEnemyKindText e
      , "pos" .= dtoEnemyPos e
      ]
        ++ optField "behaviourPreset" (dtoEnemyPreset e)
        ++ optField "behaviourHint" (dtoEnemyHint e)

instance FromJSON PickupDefDTO where
  parseJSON = withObject "Pickup" $ \o ->
    PickupDefDTO <$> o .: "id" <*> o .: "pos" <*> o .: "value"

instance ToJSON PickupDefDTO where
  toJSON p =
    object
      [ "id" .= dtoPickupId p
      , "pos" .= dtoPickupPos p
      , "value" .= dtoPickupValue p
      ]

instance FromJSON FallingHazardDefDTO where
  parseJSON = withObject "FallingHazard" $ \o ->
    FallingHazardDefDTO
      <$> o .: "id"
      <*> o .: "pos"
      <*> o .: "width"
      <*> o .: "height"
      <*> o .: "fallSpeed"
      <*> o .:? "loopDelay"

instance ToJSON FallingHazardDefDTO where
  toJSON fh =
    object $
      [ "id" .= dtoFhId fh
      , "pos" .= dtoFhPos fh
      , "width" .= dtoFhWidth fh
      , "height" .= dtoFhHeight fh
      , "fallSpeed" .= dtoFhFallSpeed fh
      ]
        ++ optField "loopDelay" (dtoFhLoopDelay fh)

instance FromJSON BossArenaDefDTO where
  parseJSON = withObject "BossArena" $ \o ->
    BossArenaDefDTO <$> o .: "left" <*> o .: "right"

instance ToJSON BossArenaDefDTO where
  toJSON ba =
    object
      [ "left" .= dtoBossLeft ba
      , "right" .= dtoBossRight ba
      ]

instance FromJSON LevelDefinitionDTO where
  parseJSON = withObject "Level" $ \o ->
    LevelDefinitionDTO
      <$> o .: "minScore"
      <*> o .: "spawn"
      <*> o .: "platforms"
      <*> o .: "movingPlatforms"
      <*> o .: "enemies"
      <*> o .: "pickups"
      <*> (fromMaybe [] <$> o .:? "fallingHazards")
      <*> (fromMaybe [] <$> o .:? "crumblingPlatforms")
      <*> o .:? "bossArena"
      <*> o .: "exit"

instance ToJSON LevelDefinitionDTO where
  toJSON lvl =
    object $
      [ "minScore" .= dtoMinScore lvl
      , "spawn" .= dtoSpawn lvl
      , "platforms" .= dtoPlatforms lvl
      , "movingPlatforms" .= dtoMovingPlatforms lvl
      , "enemies" .= dtoEnemies lvl
      , "pickups" .= dtoPickups lvl
      , "fallingHazards" .= dtoFallingHazards lvl
      , "crumblingPlatforms" .= dtoCrumblingPlatforms lvl
      , "exit" .= dtoExit lvl
      ]
        ++ optField "bossArena" (dtoBossArena lvl)

positionFromDTO :: PositionDTO -> Position
positionFromDTO dto = position (dtoPosX dto) (dtoPosY dto)

positionToDTO :: Position -> PositionDTO
positionToDTO p = PositionDTO{dtoPosX = posX p, dtoPosY = posY p}

rectFromDTO :: RectDTO -> RectDef
rectFromDTO dto =
  RectDef
    { rectPos = positionFromDTO (dtoRectPos dto)
    , rectWidth = dtoRectWidth dto
    , rectHeight = dtoRectHeight dto
    }

rectToDTO :: RectDef -> RectDTO
rectToDTO r =
  RectDTO
    { dtoRectPos = positionToDTO (rectPos r)
    , dtoRectWidth = rectWidth r
    , dtoRectHeight = rectHeight r
    }

platformFromDTO :: PlatformDefDTO -> PlatformDef
platformFromDTO (PlatformDefDTO r) = PlatformDef (rectFromDTO r)

platformToDTO :: PlatformDef -> PlatformDefDTO
platformToDTO (PlatformDef r) = PlatformDefDTO (rectToDTO r)

movingPlatformFromDTO :: MovingPlatformDefDTO -> MovingPlatformDef
movingPlatformFromDTO dto =
  MovingPlatformDef
    { mpDefId = dtoMpId dto
    , mpDefPos = positionFromDTO (dtoMpPos dto)
    , mpDefWidth = dtoMpWidth dto
    , mpDefHeight = dtoMpHeight dto
    , mpDefEndA = positionFromDTO (dtoMpEndA dto)
    , mpDefEndB = positionFromDTO (dtoMpEndB dto)
    , mpDefSpeed = dtoMpSpeed dto
    , mpDefStartTowardB = dtoMpStartTowardB dto
    }

movingPlatformToDTO :: MovingPlatformDef -> MovingPlatformDefDTO
movingPlatformToDTO mp =
  MovingPlatformDefDTO
    { dtoMpId = mpDefId mp
    , dtoMpPos = positionToDTO (mpDefPos mp)
    , dtoMpWidth = mpDefWidth mp
    , dtoMpHeight = mpDefHeight mp
    , dtoMpEndA = positionToDTO (mpDefEndA mp)
    , dtoMpEndB = positionToDTO (mpDefEndB mp)
    , dtoMpSpeed = mpDefSpeed mp
    , dtoMpStartTowardB = mpDefStartTowardB mp
    }

crumblingPlatformFromDTO :: CrumblingPlatformDefDTO -> CrumblingPlatformDef
crumblingPlatformFromDTO dto =
  CrumblingPlatformDef
    { cpDefId = dtoCpId dto
    , cpDefPos = positionFromDTO (dtoCpPos dto)
    , cpDefWidth = dtoCpWidth dto
    , cpDefHeight = dtoCpHeight dto
    }

crumblingPlatformToDTO :: CrumblingPlatformDef -> CrumblingPlatformDefDTO
crumblingPlatformToDTO cp =
  CrumblingPlatformDefDTO
    { dtoCpId = cpDefId cp
    , dtoCpPos = positionToDTO (cpDefPos cp)
    , dtoCpWidth = cpDefWidth cp
    , dtoCpHeight = cpDefHeight cp
    }

enemyFromDTO :: EnemyDefDTO -> Either LevelBuildError EnemyDef
enemyFromDTO dto = do
  kind <- parseEnemyKind (dtoEnemyKindText dto)
  mPreset <- traverse parseBehaviourArchetype (dtoEnemyPreset dto)
  pure
    EnemyDef
      { enemyDefId = dtoEnemyId dto
      , enemyDefKind = kind
      , enemyDefPos = positionFromDTO (dtoEnemyPos dto)
      , enemyDefBehaviourPreset = mPreset
      , enemyDefBehaviourHint = dtoEnemyHint dto
      , enemyDefBehaviourTuning = Nothing
      }

enemyToDTO :: EnemyDef -> EnemyDefDTO
enemyToDTO e =
  EnemyDefDTO
    { dtoEnemyId = enemyDefId e
    , dtoEnemyKindText = enemyKindToText (enemyDefKind e)
    , dtoEnemyPos = positionToDTO (enemyDefPos e)
    , dtoEnemyPreset = archetypeToText <$> enemyDefBehaviourPreset e
    , dtoEnemyHint = enemyDefBehaviourHint e
    }

pickupFromDTO :: PickupDefDTO -> PickupDef
pickupFromDTO dto =
  PickupDef
    { pickupDefId = dtoPickupId dto
    , pickupDefPos = positionFromDTO (dtoPickupPos dto)
    , pickupDefValue = dtoPickupValue dto
    }

pickupToDTO :: PickupDef -> PickupDefDTO
pickupToDTO p =
  PickupDefDTO
    { dtoPickupId = pickupDefId p
    , dtoPickupPos = positionToDTO (pickupDefPos p)
    , dtoPickupValue = pickupDefValue p
    }

fallingHazardFromDTO :: FallingHazardDefDTO -> FallingHazardDef
fallingHazardFromDTO dto =
  FallingHazardDef
    { fhDefId = dtoFhId dto
    , fhDefPos = positionFromDTO (dtoFhPos dto)
    , fhDefWidth = dtoFhWidth dto
    , fhDefHeight = dtoFhHeight dto
    , fhDefFallSpeed = dtoFhFallSpeed dto
    , fhDefLoopDelay = dtoFhLoopDelay dto
    }

fallingHazardToDTO :: FallingHazardDef -> FallingHazardDefDTO
fallingHazardToDTO fh =
  FallingHazardDefDTO
    { dtoFhId = fhDefId fh
    , dtoFhPos = positionToDTO (fhDefPos fh)
    , dtoFhWidth = fhDefWidth fh
    , dtoFhHeight = fhDefHeight fh
    , dtoFhFallSpeed = fhDefFallSpeed fh
    , dtoFhLoopDelay = fhDefLoopDelay fh
    }

bossArenaFromDTO :: BossArenaDefDTO -> BossArenaDef
bossArenaFromDTO dto =
  BossArenaDef
    { bossArenaDefLeft = dtoBossLeft dto
    , bossArenaDefRight = dtoBossRight dto
    }

bossArenaToDTO :: BossArenaDef -> BossArenaDefDTO
bossArenaToDTO ba =
  BossArenaDefDTO
    { dtoBossLeft = bossArenaDefLeft ba
    , dtoBossRight = bossArenaDefRight ba
    }

levelDefinitionFromDTO :: LevelDefinitionDTO -> Either LevelBuildError LevelDefinition
levelDefinitionFromDTO dto = do
  enemies <- traverse enemyFromDTO (dtoEnemies dto)
  pure
    LevelDefinition
      { levelMinScore = dtoMinScore dto
      , levelSpawn = positionFromDTO (dtoSpawn dto)
      , levelPlatforms = map platformFromDTO (dtoPlatforms dto)
      , levelMovingPlatforms = map movingPlatformFromDTO (dtoMovingPlatforms dto)
      , levelEnemies = enemies
      , levelPickups = map pickupFromDTO (dtoPickups dto)
      , levelFallingHazards = map fallingHazardFromDTO (dtoFallingHazards dto)
      , levelCrumblingPlatforms = map crumblingPlatformFromDTO (dtoCrumblingPlatforms dto)
      , levelBossArena = bossArenaFromDTO <$> dtoBossArena dto
      , levelExit = rectFromDTO (dtoExit dto)
      }

levelDefinitionToDTO :: LevelDefinition -> LevelDefinitionDTO
levelDefinitionToDTO lvl =
  LevelDefinitionDTO
    { dtoMinScore = levelMinScore lvl
    , dtoSpawn = positionToDTO (levelSpawn lvl)
    , dtoPlatforms = map platformToDTO (levelPlatforms lvl)
    , dtoMovingPlatforms = map movingPlatformToDTO (levelMovingPlatforms lvl)
    , dtoEnemies = map enemyToDTO (levelEnemies lvl)
    , dtoPickups = map pickupToDTO (levelPickups lvl)
    , dtoFallingHazards = map fallingHazardToDTO (levelFallingHazards lvl)
    , dtoCrumblingPlatforms = map crumblingPlatformToDTO (levelCrumblingPlatforms lvl)
    , dtoBossArena = bossArenaToDTO <$> levelBossArena lvl
    , dtoExit = rectToDTO (levelExit lvl)
    }

decodeLevelText :: Text -> Either String LevelDefinition
decodeLevelText txt =
  case eitherDecodeStrict (encodeUtf8 txt) of
    Left err -> Left ("invalid level JSON: " ++ err)
    Right dto ->
      case levelDefinitionFromDTO dto of
        Left (LevelBuildError msg) -> Left ("level codec error: " ++ unpack msg)
        Right def -> Right def

encodeLevelDefinitionText :: LevelDefinition -> Text
encodeLevelDefinitionText =
  decodeUtf8 . BL.toStrict . encode . levelDefinitionToDTO

enemyKindToText :: EnemyKind -> Text
enemyKindToText kind = case kind of
  SnailKind -> "snail"
  BatKind -> "bat"
  GolemKind -> "golem"
  ArcherKind -> "archer"
  BossGolemKind -> "bossGolem"
  BossBatKind -> "bossBat"

archetypeToText :: BehaviourArchetype -> Text
archetypeToText archetype = case archetype of
  PatrolArchetype -> "patrol"
  ChaseArchetype -> "chase"
  GuardArchetype -> "guard"
