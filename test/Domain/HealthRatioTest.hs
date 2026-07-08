module Domain.HealthRatioTest where

import Domain.ValueObjects.Health (health)
import Domain.ValueObjects.HealthRatio (healthAtOrBelowRatio, healthRatio)
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))

unit_healthRatioRejectsInvalid :: Assertion
unit_healthRatioRejectsInvalid = do
  healthRatio 0 @?= Nothing
  healthRatio (-0.1) @?= Nothing
  healthRatio 1.1 @?= Nothing

unit_healthRatioAcceptsValid :: Assertion
unit_healthRatioAcceptsValid =
  case healthRatio 0.66 of
    Just _ -> pure ()
    Nothing -> assertBool "expected valid ratio" False

unit_healthAtOrBelowRatio :: Assertion
unit_healthAtOrBelowRatio =
  case healthRatio 0.66 of
    Nothing -> assertBool "ratio" False
    Just ratio -> do
      healthAtOrBelowRatio (health 3) (health 6) ratio @?= True
      healthAtOrBelowRatio (health 4) (health 6) ratio @?= False
