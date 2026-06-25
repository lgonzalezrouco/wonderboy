-- | Clampeo de 'Amplifier' a @[1.0, 3.0]@ y fallback de no finitos.
module Domain.AmplifierTest where

import Test.Tasty.HUnit (Assertion, (@?=))

import Domain.ValueObjects.Amplifier (
  identityAmplifier,
  mkAmplifier,
  unAmplifier,
 )

unit_clampsBelowToOne :: Assertion
unit_clampsBelowToOne = unAmplifier (mkAmplifier 0.5) @?= 1.0

unit_clampsAboveToMax :: Assertion
unit_clampsAboveToMax = unAmplifier (mkAmplifier 9.0) @?= 3.0

unit_keepsValueInRange :: Assertion
unit_keepsValueInRange = unAmplifier (mkAmplifier 2.0) @?= 2.0

unit_nanFallsToIdentity :: Assertion
unit_nanFallsToIdentity = mkAmplifier (0 / 0) @?= identityAmplifier

unit_infinityFallsToIdentity :: Assertion
unit_infinityFallsToIdentity = mkAmplifier (1 / 0) @?= identityAmplifier

unit_negativeInfinityFallsToIdentity :: Assertion
unit_negativeInfinityFallsToIdentity = mkAmplifier (-(1 / 0)) @?= identityAmplifier

unit_identityIsOne :: Assertion
unit_identityIsOne = unAmplifier identityAmplifier @?= 1.0
