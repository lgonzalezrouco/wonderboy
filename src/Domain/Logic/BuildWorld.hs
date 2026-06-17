{-# LANGUAGE OverloadedStrings #-}

{- | Construcción pura de 'World' desde 'LevelDefinition'.

Valida ids duplicados y delega en smart constructors de plataformas,
pickups y enemigos.
-}
module Domain.Logic.BuildWorld (
  buildWorld,
)
where

import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T

import Domain.Logic.EntityBehaviours (defaultProgramForKind, programForArchetype)
import Domain.Model.Enemy (Enemy, spawnEnemy)
import Domain.Model.ExitZone (ExitZone (..))
import Domain.Model.LevelDefinition (
  EnemyDef (..),
  LevelBuildError (..),
  LevelDefinition (..),
  MovingPlatformDef (..),
  PickupDef (..),
  PlatformDef (..),
  RectDef (..),
  levelBuildError,
 )
import Domain.Model.MovingPlatform (MovingPlatform, mkMovingPlatform)
import Domain.Model.Pickup (Pickup, mkPickup)
import Domain.Model.Platform (Platform, platform)
import Domain.Model.Player (spawnPlayer)
import Domain.Model.World (World (..), defaultMaxHealth)

-- | Construye el mundo inicial del nivel a partir de la definición autoral.
buildWorld :: LevelDefinition -> Either LevelBuildError World
buildWorld lvl = do
  checkMinScore (levelMinScore lvl)
  checkUniqueIds (map mpDefId (levelMovingPlatforms lvl)) "moving platform"
  checkUniqueIds (map enemyDefId (levelEnemies lvl)) "enemy"
  checkUniqueIds (map pickupDefId (levelPickups lvl)) "pickup"
  movingPlats <- traverse buildMovingPlatform (levelMovingPlatforms lvl)
  enemies <- traverse buildEnemy (levelEnemies lvl)
  pickups <- traverse buildPickup (levelPickups lvl)
  let spawn = levelSpawn lvl
  pure
    World
      { worldPlayer = spawnPlayer defaultMaxHealth spawn
      , worldEnemies = enemies
      , worldPlatforms = map buildPlatform (levelPlatforms lvl)
      , worldMovingPlatforms = movingPlats
      , worldSpawnPoint = spawn
      , worldPickups = pickups
      , worldMinScore = levelMinScore lvl
      , worldExit = buildExit (levelExit lvl)
      }

checkUniqueIds :: [Int] -> Text -> Either LevelBuildError ()
checkUniqueIds ids label
  | length ids == length (nub ids) = Right ()
  | otherwise =
      Left (levelBuildError ("duplicate " <> label <> " id"))

checkMinScore :: Int -> Either LevelBuildError ()
checkMinScore n
  | n >= 0 = Right ()
  | otherwise = Left (levelBuildError "minScore must be >= 0")

buildPlatform :: PlatformDef -> Platform
buildPlatform (PlatformDef rect) =
  platform (rectPos rect) (rectWidth rect) (rectHeight rect)

buildMovingPlatform :: MovingPlatformDef -> Either LevelBuildError MovingPlatform
buildMovingPlatform mp =
  case mkMovingPlatform
    (mpDefId mp)
    (mpDefPos mp)
    (mpDefWidth mp)
    (mpDefHeight mp)
    (mpDefEndA mp)
    (mpDefEndB mp)
    (mpDefSpeed mp)
    (mpDefStartTowardB mp) of
    Nothing -> Left (invalidBuildId "invalid moving platform id " (mpDefId mp))
    Just plat -> Right plat

buildEnemy :: EnemyDef -> Either LevelBuildError Enemy
buildEnemy def =
  let prog =
        case enemyDefBehaviourPreset def of
          Just archetype -> programForArchetype (enemyDefKind def) archetype
          Nothing -> defaultProgramForKind (enemyDefKind def)
   in pure $
        spawnEnemy
          (enemyDefId def)
          (enemyDefKind def)
          (enemyDefPos def)
          prog

buildPickup :: PickupDef -> Either LevelBuildError Pickup
buildPickup def =
  case mkPickup (pickupDefId def) (pickupDefPos def) (pickupDefValue def) of
    Nothing -> Left (invalidBuildId "invalid pickup id " (pickupDefId def))
    Just p -> Right p

buildExit :: RectDef -> ExitZone
buildExit rect =
  ExitZone
    { exitPos = rectPos rect
    , exitWidth = rectWidth rect
    , exitHeight = rectHeight rect
    }

invalidBuildId :: Text -> Int -> LevelBuildError
invalidBuildId prefix entityId =
  levelBuildError (prefix <> T.pack (show entityId))
