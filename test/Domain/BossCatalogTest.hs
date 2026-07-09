module Domain.BossCatalogTest where

import Data.Maybe (mapMaybe)

import Test.Tasty.HUnit (Assertion, assertBool)

import Domain.Logic.BossCatalog (bossDefinitionForKind)
import Domain.Model.BossPhase (
  BossDefinition (..),
  BossPhaseCondition (..),
  BossPhaseDef (..),
 )
import Domain.Model.EnemyKind (EnemyKind (..))
import Domain.ValueObjects.HealthRatio (healthRatioValue)

phaseRatios :: BossDefinition -> [Float]
phaseRatios def =
  [ healthRatioValue ratio
  | phase <- bossPhases def
  , HealthAtOrBelowRatio ratio <- phaseConditions phase
  ]

unit_bossPhaseRatiosAreValidThresholds :: Assertion
unit_bossPhaseRatiosAreValidThresholds =
  assertBool
    "boss phase ratios must lie strictly within (0, 1)"
    (all valid ratios)
 where
  ratios = concatMap phaseRatios definitions
  definitions = mapMaybe bossDefinitionForKind [BossGolemKind, BossBatKind]
  valid r = r > 0 && r < 1
