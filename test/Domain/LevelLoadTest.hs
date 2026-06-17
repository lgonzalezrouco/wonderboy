{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | JSON level definition decode, build, and validation tests.
module Domain.LevelLoadTest where

import Data.Aeson (decode, eitherDecodeStrict, encode)
import Data.Text.Encoding (encodeUtf8)
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
    case decode (encode lvl) of
      Nothing -> assertFailure "round trip decode failed"
      Just lvl' -> do
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
  let bad =
        "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[],\"enemies\":[{\"id\":1,\"kind\":\"dragon\",\"pos\":{\"x\":0,\"y\":0}}],\"pickups\":[],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
   in case eitherDecodeStrict @LevelDefinition (encodeUtf8 bad) of
        Left _ -> pure ()
        Right _ -> assertFailure "expected decode failure for unknown kind"

unit_rejectDuplicateEnemyId :: Assertion
unit_rejectDuplicateEnemyId =
  let bad =
        "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[],\"enemies\":[{\"id\":1,\"kind\":\"snail\",\"pos\":{\"x\":0,\"y\":0}},{\"id\":1,\"kind\":\"bat\",\"pos\":{\"x\":10,\"y\":0}}],\"pickups\":[],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
   in case eitherDecodeStrict @LevelDefinition (encodeUtf8 bad) of
        Left err -> assertFailure err
        Right lvl ->
          case buildWorld lvl of
            Left (LevelBuildError _) -> pure ()
            _ -> assertFailure "expected LevelBuildError for duplicate enemy id"

unit_rejectBadMovingPlatform :: Assertion
unit_rejectBadMovingPlatform =
  let bad =
        "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[{\"id\":1,\"pos\":{\"x\":0,\"y\":0},\"width\":8,\"height\":8,\"endA\":{\"x\":0,\"y\":0},\"endB\":{\"x\":10,\"y\":5},\"speed\":10,\"startTowardB\":true}],\"enemies\":[],\"pickups\":[],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
   in case eitherDecodeStrict @LevelDefinition (encodeUtf8 bad) of
        Left err -> assertFailure err
        Right lvl ->
          case buildWorld lvl of
            Left (LevelBuildError _) -> pure ()
            _ -> assertFailure "expected LevelBuildError for diagonal moving platform"

unit_rejectMalformedJson :: Assertion
unit_rejectMalformedJson =
  case eitherDecodeStrict @LevelDefinition (encodeUtf8 "not json") of
    Left _ -> pure ()
    Right _ -> assertFailure "expected malformed JSON to fail decode"

unit_rejectNegativeMinScore :: Assertion
unit_rejectNegativeMinScore =
  let bad =
        "{\"minScore\":-1,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[],\"enemies\":[],\"pickups\":[],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
   in case eitherDecodeStrict @LevelDefinition (encodeUtf8 bad) of
        Left err -> assertFailure err
        Right lvl ->
          case buildWorld lvl of
            Left (LevelBuildError _) -> pure ()
            _ -> assertFailure "expected LevelBuildError for negative minScore"
