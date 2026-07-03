{-# LANGUAGE OverloadedStrings #-}

-- | JSON level definition decode, build, and validation tests.
module Domain.LevelLoadTest where

import Data.Text (Text)
import Domain.Fixtures (decodeDemoLevel)
import Domain.Logic.BuildWorld (buildWorld)
import Domain.Model.LevelDefinition (
  LevelBuildError (..),
  LevelDefinition (..),
  levelEnemies,
  levelMinScore,
 )
import Domain.Model.World (World (..))
import Domain.ValueObjects.Score (score)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))
import UseCases.Serialization.LevelCodec (decodeLevelText, encodeLevelDefinitionText)

withDecodedFixture :: (LevelDefinition -> Assertion) -> Assertion
withDecodedFixture f =
  case decodeDemoLevel of
    Left err -> assertFailure err
    Right lvl -> f lvl

unit_decodeDemoFixture :: Assertion
unit_decodeDemoFixture =
  withDecodedFixture $ \lvl -> levelMinScore lvl @?= 150

unit_roundTripDemoFields :: Assertion
unit_roundTripDemoFields =
  withDecodedFixture $ \lvl ->
    case decodeLevelText (encodeLevelDefinitionText lvl) of
      Left err -> assertFailure ("round trip decode failed: " ++ err)
      Right lvl' -> do
        levelMinScore lvl' @?= levelMinScore lvl
        length (levelEnemies lvl') @?= length (levelEnemies lvl)

unit_buildWorldDemo :: Assertion
unit_buildWorldDemo =
  withDecodedFixture $ \lvl ->
    case buildWorld lvl of
      Left (LevelBuildError msg) -> assertFailure (show msg)
      Right w -> do
        length (worldEnemies w) @?= 3
        worldMinScore w @?= score 150
        assertBool "at least three platforms" (length (worldPlatforms w) >= 3)

unit_rejectUnknownKind :: Assertion
unit_rejectUnknownKind =
  let bad :: Text
      bad = "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[],\"enemies\":[{\"id\":1,\"kind\":\"dragon\",\"pos\":{\"x\":0,\"y\":0}}],\"pickups\":[],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
   in case decodeLevelText bad of
        Left _ -> pure ()
        Right _ -> assertFailure "expected decode failure for unknown kind"

unit_rejectDuplicateEnemyId :: Assertion
unit_rejectDuplicateEnemyId =
  let bad :: Text
      bad = "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[],\"enemies\":[{\"id\":1,\"kind\":\"snail\",\"pos\":{\"x\":0,\"y\":0}},{\"id\":1,\"kind\":\"bat\",\"pos\":{\"x\":10,\"y\":0}}],\"pickups\":[],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
   in case decodeLevelText bad of
        Left err -> assertFailure err
        Right lvl ->
          case buildWorld lvl of
            Left (LevelBuildError _) -> pure ()
            _ -> assertFailure "expected LevelBuildError for duplicate enemy id"

unit_rejectBadMovingPlatform :: Assertion
unit_rejectBadMovingPlatform =
  let bad :: Text
      bad = "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[{\"id\":1,\"pos\":{\"x\":0,\"y\":0},\"width\":8,\"height\":8,\"endA\":{\"x\":0,\"y\":0},\"endB\":{\"x\":10,\"y\":5},\"speed\":10,\"startTowardB\":true}],\"enemies\":[],\"pickups\":[],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
   in case decodeLevelText bad of
        Left err -> assertFailure err
        Right lvl ->
          case buildWorld lvl of
            Left (LevelBuildError _) -> pure ()
            _ -> assertFailure "expected LevelBuildError for diagonal moving platform"

unit_rejectMalformedJson :: Assertion
unit_rejectMalformedJson =
  case decodeLevelText "not json" of
    Left _ -> pure ()
    Right _ -> assertFailure "expected malformed JSON to fail decode"

unit_rejectNegativeMinScore :: Assertion
unit_rejectNegativeMinScore =
  let bad :: Text
      bad = "{\"minScore\":-1,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[],\"enemies\":[],\"pickups\":[],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
   in case decodeLevelText bad of
        Left err -> assertFailure err
        Right lvl ->
          case buildWorld lvl of
            Left (LevelBuildError _) -> pure ()
            _ -> assertFailure "expected LevelBuildError for negative minScore"

unit_bossGolemDecode :: Assertion
unit_bossGolemDecode =
  let json :: Text
      json = "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[],\"enemies\":[{\"id\":1,\"kind\":\"bossGolem\",\"pos\":{\"x\":0,\"y\":0}}],\"pickups\":[],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
   in case decodeLevelText json of
        Left err -> assertFailure err
        Right lvl ->
          case buildWorld lvl of
            Left (LevelBuildError msg) -> assertFailure (show msg)
            Right w -> length (worldEnemies w) @?= 1

unit_bossRejectsBehaviourPreset :: Assertion
unit_bossRejectsBehaviourPreset =
  let json :: Text
      json = "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[],\"enemies\":[{\"id\":1,\"kind\":\"bossGolem\",\"pos\":{\"x\":0,\"y\":0},\"behaviourPreset\":\"patrol\"}],\"pickups\":[],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
   in case decodeLevelText json of
        Left err -> assertFailure err
        Right lvl ->
          case buildWorld lvl of
            Left (LevelBuildError _) -> pure ()
            _ -> assertFailure "expected LevelBuildError for boss behaviourPreset"

unit_bossRejectsDuplicateBoss :: Assertion
unit_bossRejectsDuplicateBoss =
  let json :: Text
      json = "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[],\"enemies\":[{\"id\":1,\"kind\":\"bossGolem\",\"pos\":{\"x\":0,\"y\":0}},{\"id\":2,\"kind\":\"bossBat\",\"pos\":{\"x\":10,\"y\":0}}],\"pickups\":[],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
   in case decodeLevelText json of
        Left err -> assertFailure err
        Right lvl ->
          case buildWorld lvl of
            Left (LevelBuildError _) -> pure ()
            _ -> assertFailure "expected LevelBuildError for duplicate boss"
