{- | Tests de 'scaleHealth': escala la salud por un factor con piso de 1 HP, para que
@toughness×@ nunca deje a un enemigo naciendo derrotado.
-}
module Domain.ScaleHealthTest where

import Test.Tasty.HUnit (Assertion, (@?=))

import Domain.ValueObjects.Health (health, healthPoints, scaleHealth)

unit_scaleUpRounds :: Assertion
unit_scaleUpRounds = healthPoints (scaleHealth 2.5 (health 2)) @?= 5

unit_scaleDownFloorsAtOne :: Assertion
unit_scaleDownFloorsAtOne = healthPoints (scaleHealth 0.3 (health 1)) @?= 1

unit_identityKeepsHealth :: Assertion
unit_identityKeepsHealth = healthPoints (scaleHealth 1.0 (health 2)) @?= 2
