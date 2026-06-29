-- | Verifica que los umbrales de fase del catálogo de jefes sean válidos.
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

-- | Ratios de todas las condiciones @HealthAtOrBelowRatio@ de una definición.
phaseRatios :: BossDefinition -> [Float]
phaseRatios def =
  [ healthRatioValue ratio
  | phase <- bossPhases def
  , HealthAtOrBelowRatio ratio <- phaseConditions phase
  ]

{- | Todo umbral de fase debe quedar estrictamente dentro de (0, 1).

Un literal inválido en 'Domain.Logic.BossCatalog' caería a 'maxHealthRatio'
(1.0) y la fase dispararía a salud completa; este test detecta ese caso en CI en
vez de dejarlo pasar en silencio.
-}
unit_bossPhaseRatiosAreValidThresholds :: Assertion
unit_bossPhaseRatiosAreValidThresholds =
  assertBool
    "boss phase ratios must lie strictly within (0, 1)"
    (all valid ratios)
 where
  ratios = concatMap phaseRatios definitions
  definitions = mapMaybe bossDefinitionForKind [BossGolemKind, BossBatKind]
  valid r = r > 0 && r < 1
