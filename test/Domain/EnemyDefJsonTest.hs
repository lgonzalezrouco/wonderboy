{-# LANGUAGE OverloadedStrings #-}

module Domain.EnemyDefJsonTest where

import Data.Text (Text)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))
import UseCases.Serialization.LevelCodec (decodeLevelText)

import Domain.Model.LevelDefinition (EnemyDef (..), levelEnemies)

unit_tuningDefaultsToNothing :: Assertion
unit_tuningDefaultsToNothing =
  let json :: Text
      json = "{\"minScore\":0,\"spawn\":{\"x\":0,\"y\":0},\"platforms\":[],\"movingPlatforms\":[],\"enemies\":[{\"id\":1,\"kind\":\"snail\",\"pos\":{\"x\":0,\"y\":0}}],\"pickups\":[],\"exit\":{\"pos\":{\"x\":0,\"y\":0},\"width\":1,\"height\":1}}"
   in case decodeLevelText json of
        Left err -> assertFailure ("decode failed: " ++ err)
        Right lvl ->
          case levelEnemies lvl of
            [enemy] -> enemyDefBehaviourTuning enemy @?= Nothing
            _ -> assertFailure "expected exactly one enemy"
