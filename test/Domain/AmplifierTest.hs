{- | Tests del value object 'Amplifier': clampea su factor a @[1.0, 3.0]@ (piso 1.0,
solo amplifica, nunca reduce) y los valores no finitos caen a la identidad.
-}
module Domain.AmplifierTest where

import Test.Tasty.HUnit (Assertion, (@?=))

import Domain.ValueObjects.Amplifier (
  identityAmplifier,
  mkAmplifier,
  unAmplifier,
 )

-- | Un valor < 1 (que reduciría) se clampa al piso 1.0: el Amplifier nunca achica.
unit_clampsBelowToOne :: Assertion
unit_clampsBelowToOne = unAmplifier (mkAmplifier 0.5) @?= 1.0

-- | Un valor por encima del techo se clampa a 3.0.
unit_clampsAboveToMax :: Assertion
unit_clampsAboveToMax = unAmplifier (mkAmplifier 9.0) @?= 3.0

-- | Un valor dentro de rango se conserva tal cual.
unit_keepsValueInRange :: Assertion
unit_keepsValueInRange = unAmplifier (mkAmplifier 2.0) @?= 2.0

-- | NaN cae a la identidad (1.0): las comparaciones con NaN son False y el clamp no alcanza.
unit_nanFallsToIdentity :: Assertion
unit_nanFallsToIdentity = mkAmplifier (0 / 0) @?= identityAmplifier

-- | +Infinity cae a la identidad.
unit_infinityFallsToIdentity :: Assertion
unit_infinityFallsToIdentity = mkAmplifier (1 / 0) @?= identityAmplifier

-- | -Infinity cae a la identidad.
unit_negativeInfinityFallsToIdentity :: Assertion
unit_negativeInfinityFallsToIdentity = mkAmplifier (-(1 / 0)) @?= identityAmplifier

-- | La identidad es 1.0 (sin amplificar).
unit_identityIsOne :: Assertion
unit_identityIsOne = unAmplifier identityAmplifier @?= 1.0
