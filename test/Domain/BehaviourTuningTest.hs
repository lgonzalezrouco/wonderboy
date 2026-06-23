{- | Tests de los value objects de tuning: 'Multiplier' clampea su factor a un rango
seguro y 'identityTuning' es el "sin ajuste" (todo ×1.0).
-}
module Domain.BehaviourTuningTest where

import Test.Tasty.HUnit (Assertion, (@?=))

import Domain.ValueObjects.Amplifier (identityAmplifier)
import Domain.ValueObjects.BehaviourTuning (
  BehaviourTuning (..),
  identityTuning,
 )
import Domain.ValueObjects.Multiplier (
  identityMultiplier,
  mkMultiplier,
  unMultiplier,
 )

unit_clampsBelowToMin :: Assertion
unit_clampsBelowToMin = unMultiplier (mkMultiplier 0.05) @?= 0.3

unit_clampsAboveToMax :: Assertion
unit_clampsAboveToMax = unMultiplier (mkMultiplier 9.0) @?= 3.0

unit_keepsValueInRange :: Assertion
unit_keepsValueInRange = unMultiplier (mkMultiplier 1.5) @?= 1.5

unit_nanFallsToIdentity :: Assertion
unit_nanFallsToIdentity = mkMultiplier (0 / 0) @?= identityMultiplier

unit_infinityFallsToIdentity :: Assertion
unit_infinityFallsToIdentity = mkMultiplier (1 / 0) @?= identityMultiplier

unit_negativeInfinityFallsToIdentity :: Assertion
unit_negativeInfinityFallsToIdentity = mkMultiplier (-(1 / 0)) @?= identityMultiplier

unit_identityIsOne :: Assertion
unit_identityIsOne = unMultiplier identityMultiplier @?= 1.0

unit_identityTuningIsAllOnes :: Assertion
unit_identityTuningIsAllOnes =
  ( tuningSpeed identityTuning
  , tuningReach identityTuning
  , tuningToughness identityTuning
  )
    @?= (identityMultiplier, identityAmplifier, identityAmplifier)
