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

import Domain.Logic.BossCatalog (bossDefinitionForKind)
import Domain.Logic.EntityBehaviours (defaultProgramForKind, programForArchetype)
import Domain.Model.BossPhase (BossDefinition (..), BossPhaseDef (..), bossMaxHealth, bossPhases, phaseProgram)
import Domain.Model.Enemy (Enemy, enemyHealth, enemyMaxHealth, spawnEnemy)
import Domain.Model.EnemyKind (isBossKind)
import Domain.Model.ExitZone (ExitZone (..))
import Domain.Model.FallingHazard (FallingHazard, spawnFallingHazard)
import Domain.Model.LevelDefinition (
  EnemyDef (..),
  FallingHazardDef (..),
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
import Domain.ValueObjects.Frames (frames)
import Domain.ValueObjects.Score (score)

-- | Construye el mundo inicial del nivel a partir de la definición autoral.
buildWorld :: LevelDefinition -> Either LevelBuildError World
buildWorld lvl = do
  checkMinScore (levelMinScore lvl)
  checkUniqueIds (map mpDefId (levelMovingPlatforms lvl)) "moving platform"
  checkUniqueIds (map enemyDefId (levelEnemies lvl)) "enemy"
  checkUniqueIds (map pickupDefId (levelPickups lvl)) "pickup"
  checkUniqueIds (map fhDefId (levelFallingHazards lvl)) "falling hazard"
  checkBossCount (levelEnemies lvl)
  movingPlats <- traverse buildMovingPlatform (levelMovingPlatforms lvl)
  enemies <- traverse buildEnemy (levelEnemies lvl)
  pickups <- traverse buildPickup (levelPickups lvl)
  hazards <- traverse buildFallingHazard (levelFallingHazards lvl)
  let spawn = levelSpawn lvl
  pure
    World
      { worldPlayer = spawnPlayer defaultMaxHealth spawn
      , worldEnemies = enemies
      , worldPlatforms = map buildPlatform (levelPlatforms lvl)
      , worldMovingPlatforms = movingPlats
      , worldSpawnPoint = spawn
      , worldPickups = pickups
      , worldMinScore = score (levelMinScore lvl)
      , worldExit = buildExit (levelExit lvl)
      , worldProjectiles = []
      , worldNextProjectileId = 1
      , worldFallingHazards = hazards
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

checkBossCount :: [EnemyDef] -> Either LevelBuildError ()
checkBossCount defs
  | length bossDefs <= 1 = Right ()
  | otherwise = Left (levelBuildError "at most one boss per level")
 where
  bossDefs = filter (isBossKind . enemyDefKind) defs

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
buildEnemy def
  | isBossKind (enemyDefKind def) = buildBossEnemy def
  | otherwise =
      pure $
        spawnEnemy
          (enemyDefId def)
          (enemyDefKind def)
          (enemyDefPos def)
          (behaviourProgramFor def)
 where
  behaviourProgramFor d =
    case enemyDefBehaviourPreset d of
      Just archetype -> programForArchetype (enemyDefKind d) archetype
      Nothing -> defaultProgramForKind (enemyDefKind d)

buildBossEnemy :: EnemyDef -> Either LevelBuildError Enemy
buildBossEnemy def
  | Just _ <- enemyDefBehaviourPreset def =
      Left (levelBuildError "boss enemies cannot use behaviourPreset")
  | Just _ <- enemyDefBehaviourHint def =
      Left (levelBuildError "boss enemies cannot use behaviourHint")
  | otherwise =
      case bossDefinitionForKind (enemyDefKind def) of
        Nothing ->
          Left (levelBuildError "missing boss catalog entry for kind")
        Just bossDef ->
          case bossPhases bossDef of
            (phase0 : _) ->
              let spawned =
                    spawnEnemy
                      (enemyDefId def)
                      (enemyDefKind def)
                      (enemyDefPos def)
                      (phaseProgram phase0)
                  maxHp = bossMaxHealth bossDef
               in pure spawned{enemyHealth = maxHp, enemyMaxHealth = maxHp}
            [] ->
              Left (levelBuildError "boss catalog entry has no phases")

buildPickup :: PickupDef -> Either LevelBuildError Pickup
buildPickup def =
  case mkPickup (pickupDefId def) (pickupDefPos def) (pickupDefValue def) of
    Nothing -> Left (invalidBuildId "invalid pickup id " (pickupDefId def))
    Just p -> Right p

buildFallingHazard :: FallingHazardDef -> Either LevelBuildError FallingHazard
buildFallingHazard def
  | fhDefFallSpeed def <= 0 =
      Left (levelBuildError "fallSpeed must be > 0")
  | fhDefWidth def <= 0 || fhDefHeight def <= 0 =
      Left (levelBuildError "falling hazard width and height must be > 0")
  | Just delay <- fhDefLoopDelay def
  , delay < 0 =
      Left (levelBuildError "loopDelay must be >= 0")
  | otherwise =
      Right $
        spawnFallingHazard
          (fhDefId def)
          (fhDefPos def)
          (fhDefWidth def)
          (fhDefHeight def)
          (fhDefFallSpeed def)
          (frames <$> fhDefLoopDelay def)

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
