{-# LANGUAGE OverloadedStrings #-}

module UseCases.LoadLevelTest where

import Data.List (isInfixOf)

import Domain.Fixtures (demoJsonFixture)
import Domain.Model.World (worldEnemies, worldMinScore)
import Domain.ValueObjects.Score (score)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))
import UseCases.GameMonad (GameError (..))
import UseCases.LoadLevel (loadLevelFromText)

unit_loadValidLevelYieldsWorld :: Assertion
unit_loadValidLevelYieldsWorld =
  case loadLevelFromText demoJsonFixture of
    Left (GameError err) -> assertFailure err
    Right w -> do
      length (worldEnemies w) @?= 3
      worldMinScore w @?= score 150

unit_loadMalformedJsonYieldsDecodeError :: Assertion
unit_loadMalformedJsonYieldsDecodeError =
  case loadLevelFromText "not json" of
    Left (GameError msg) ->
      assertBool "decode error is labelled" ("invalid level JSON" `isInfixOf` msg)
    Right _ -> assertFailure "expected malformed JSON to fail"

unit_loadDuplicateEnemyIdYieldsBuildError :: Assertion
unit_loadDuplicateEnemyIdYieldsBuildError =
  case loadLevelFromText dupEnemyIdJson of
    Left (GameError msg) ->
      assertBool "build error is labelled" ("level build error" `isInfixOf` msg)
    Right _ -> assertFailure "expected duplicate enemy id to fail build"
 where
  dupEnemyIdJson =
    "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[],\"enemies\":[{\"id\":1,\"kind\":\"snail\",\"pos\":{\"x\":0,\"y\":0}},{\"id\":1,\"kind\":\"bat\",\"pos\":{\"x\":10,\"y\":0}}],\"pickups\":[],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
