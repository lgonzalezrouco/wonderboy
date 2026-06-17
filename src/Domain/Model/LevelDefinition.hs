{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Esquema de definición de nivel (contenido autoral, no estado de runtime).

Tipos alineados con @levels/*.json@; la construcción del 'World' vive en
@Domain.Logic.BuildWorld@.
-}
module Domain.Model.LevelDefinition (
  BehaviourArchetype (..),
  EnemyDef (..),
  LevelBuildError (..),
  LevelDefinition (..),
  MovingPlatformDef (..),
  PickupDef (..),
  PlatformDef (..),
  RectDef (..),
  levelBuildError,
  parseBehaviourArchetype,
  parseEnemyKind,
)
where

import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  object,
  withObject,
  (.:),
  (.:?),
  (.=),
 )
import Data.Aeson.Types (Parser)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)

import Domain.Model.EnemyKind (EnemyKind (..))
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
  deriving newtype (FromJSON, ToJSON)

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

-- | Colocación de enemigo en el JSON del nivel.
data EnemyDef = EnemyDef
  { enemyDefId :: Int
  , enemyDefKind :: EnemyKind
  , enemyDefPos :: Position
  , enemyDefBehaviourPreset :: Maybe BehaviourArchetype
  , enemyDefBehaviourHint :: Maybe Text
  }
  deriving (Eq, Show, Generic)

-- | Colocación de pickup en el JSON del nivel.
data PickupDef = PickupDef
  { pickupDefId :: Int
  , pickupDefPos :: Position
  , pickupDefValue :: Int
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
  , levelExit :: RectDef
  }
  deriving (Eq, Show, Generic)

instance FromJSON RectDef where
  parseJSON = withObject "Rect" $ \o ->
    RectDef <$> o .: "pos" <*> o .: "width" <*> o .: "height"

instance ToJSON RectDef where
  toJSON r =
    object
      [ "pos" .= rectPos r
      , "width" .= rectWidth r
      , "height" .= rectHeight r
      ]

instance FromJSON MovingPlatformDef where
  parseJSON = withObject "MovingPlatform" $ \o ->
    MovingPlatformDef
      <$> o .: "id"
      <*> o .: "pos"
      <*> o .: "width"
      <*> o .: "height"
      <*> o .: "endA"
      <*> o .: "endB"
      <*> o .: "speed"
      <*> o .: "startTowardB"

instance ToJSON MovingPlatformDef where
  toJSON mp =
    object
      [ "id" .= mpDefId mp
      , "pos" .= mpDefPos mp
      , "width" .= mpDefWidth mp
      , "height" .= mpDefHeight mp
      , "endA" .= mpDefEndA mp
      , "endB" .= mpDefEndB mp
      , "speed" .= mpDefSpeed mp
      , "startTowardB" .= mpDefStartTowardB mp
      ]

instance FromJSON EnemyDef where
  parseJSON = withObject "Enemy" $ \o -> do
    kind <- o .: "kind" >>= parseOrFail parseEnemyKind
    preset <- o .:? "behaviourPreset" >>= traverse (parseOrFail parseBehaviourArchetype)
    EnemyDef <$> o .: "id" <*> pure kind <*> o .: "pos" <*> pure preset <*> o .:? "behaviourHint"

instance ToJSON EnemyDef where
  toJSON e =
    object $
      [ "id" .= enemyDefId e
      , "kind" .= enemyKindToText (enemyDefKind e)
      , "pos" .= enemyDefPos e
      ]
        ++ maybe [] (\p -> ["behaviourPreset" .= archetypeToText p]) (enemyDefBehaviourPreset e)
        ++ maybe [] (\h -> ["behaviourHint" .= h]) (enemyDefBehaviourHint e)

instance FromJSON PickupDef where
  parseJSON = withObject "Pickup" $ \o ->
    PickupDef <$> o .: "id" <*> o .: "pos" <*> o .: "value"

instance ToJSON PickupDef where
  toJSON p =
    object
      [ "id" .= pickupDefId p
      , "pos" .= pickupDefPos p
      , "value" .= pickupDefValue p
      ]

instance FromJSON LevelDefinition where
  parseJSON = withObject "Level" $ \o ->
    LevelDefinition
      <$> o .: "minScore"
      <*> o .: "spawn"
      <*> o .: "platforms"
      <*> o .: "movingPlatforms"
      <*> o .: "enemies"
      <*> o .: "pickups"
      <*> o .: "exit"

instance ToJSON LevelDefinition where
  toJSON lvl =
    object
      [ "minScore" .= levelMinScore lvl
      , "spawn" .= levelSpawn lvl
      , "platforms" .= levelPlatforms lvl
      , "movingPlatforms" .= levelMovingPlatforms lvl
      , "enemies" .= levelEnemies lvl
      , "pickups" .= levelPickups lvl
      , "exit" .= levelExit lvl
      ]

enemyKindToText :: EnemyKind -> Text
enemyKindToText kind = case kind of
  SnailKind -> "snail"
  BatKind -> "bat"
  GolemKind -> "golem"
  BossGolemKind -> "bossGolem"
  BossBatKind -> "bossBat"

archetypeToText :: BehaviourArchetype -> Text
archetypeToText archetype = case archetype of
  PatrolArchetype -> "patrol"
  ChaseArchetype -> "chase"
  GuardArchetype -> "guard"

-- | Parsea clase de enemigo desde texto de nivel.
parseEnemyKind :: Text -> Either LevelBuildError EnemyKind
parseEnemyKind txt = case txt of
  "snail" -> Right SnailKind
  "bat" -> Right BatKind
  "golem" -> Right GolemKind
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

parseOrFail :: (Text -> Either LevelBuildError a) -> Text -> Parser a
parseOrFail p txt =
  case p txt of
    Right value -> pure value
    Left (LevelBuildError msg) -> fail (unpack msg)
