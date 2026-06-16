{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | JSON level definition decode, build, and validation tests.
module Domain.LevelLoadTest where

import Data.Aeson (decode, eitherDecodeStrict, encode)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Domain.Logic.BuildWorld (buildWorld)
import Domain.Model.LevelDefinition (
  LevelBuildError (..),
  LevelDefinition (..),
  levelEnemies,
  levelMinScore,
 )
import Domain.Model.World (World (..))
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))

demoJsonFixture :: Text
demoJsonFixture =
  "{\"minScore\":150,\"spawn\":{\"x\":-100,\"y\":80},\"platforms\":[{\"pos\":{\"x\":-200,\"y\":0},\"width\":400,\"height\":8},{\"pos\":{\"x\":130,\"y\":24},\"width\":32,\"height\":8},{\"pos\":{\"x\":200,\"y\":48},\"width\":64,\"height\":8}],\"movingPlatforms\":[{\"id\":1,\"pos\":{\"x\":30,\"y\":72},\"width\":48,\"height\":8,\"endA\":{\"x\":30,\"y\":72},\"endB\":{\"x\":90,\"y\":72},\"speed\":35,\"startTowardB\":true}],\"enemies\":[{\"id\":1,\"kind\":\"snail\",\"pos\":{\"x\":40,\"y\":8}},{\"id\":2,\"kind\":\"bat\",\"pos\":{\"x\":80,\"y\":8}},{\"id\":3,\"kind\":\"golem\",\"pos\":{\"x\":170,\"y\":8}}],\"pickups\":[{\"id\":1,\"pos\":{\"x\":-120,\"y\":8},\"value\":100},{\"id\":2,\"pos\":{\"x\":10,\"y\":8},\"value\":50},{\"id\":3,\"pos\":{\"x\":60,\"y\":80},\"value\":200},{\"id\":4,\"pos\":{\"x\":232,\"y\":56},\"value\":75}],\"exit\":{\"pos\":{\"x\":280,\"y\":0},\"width\":32,\"height\":64}}"

decodeFixture :: Either String LevelDefinition
decodeFixture = eitherDecodeStrict @LevelDefinition (encodeUtf8 demoJsonFixture)

withDecodedFixture :: (LevelDefinition -> Assertion) -> Assertion
withDecodedFixture f =
  case decodeFixture of
    Left err -> assertFailure err
    Right lvl -> f lvl

demoWorld :: World
demoWorld =
  case buildWorld <$> decodeFixture of
    Right (Right w) -> w
    Right (Left (LevelBuildError msg)) -> error (show msg)
    Left err -> error err

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
        worldMinScore w @?= 150
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
