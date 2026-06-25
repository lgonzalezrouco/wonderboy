{- | Tests de 'scaleHealth': escala la salud por un factor redondeando hacia arriba
('ceiling'), con piso de 1 HP, para que @toughness×@ nunca deje a un enemigo naciendo
derrotado y para que cualquier factor > 1.0 cueste al menos +1 HP sobre una base chica.
-}
module Domain.ScaleHealthTest where

import Test.Tasty.HUnit (Assertion, (@?=))

import Domain.ValueObjects.Health (health, healthPoints, scaleHealth)

{- | Un factor apenas > 1 sobre 1 HP siempre suma al menos 1 golpe: con 'round',
@scaleHealth 1.2 (health 1)@ volvía a 1 (la amplificación se perdía); con 'ceiling' da 2.
-}
unit_amplifyOneHpCeils :: Assertion
unit_amplifyOneHpCeils = healthPoints (scaleHealth 1.2 (health 1)) @?= 2

{- | El máximo realista que devuelve el modelo (toughness 2.5) sobre 1 HP da 3 HP con
'ceiling'; con 'round' (banker's, round-half-to-even) @round 2.5 = 2@ lo dejaba en 2.
-}
unit_amplifyMaxOneHp :: Assertion
unit_amplifyMaxOneHp = healthPoints (scaleHealth 2.5 (health 1)) @?= 3

-- | 'ceiling' sobre base mayor: @1.5×@ de 3 HP = @ceiling 4.5 = 5@ (con 'round' habría dado 4).
unit_scaleUpCeils :: Assertion
unit_scaleUpCeils = healthPoints (scaleHealth 1.5 (health 3)) @?= 5

-- | Factor <1 satura en el piso de 1 HP (caso sintético: el 'Amplifier' nunca baja de 1.0).
unit_scaleDownFloorsAtOne :: Assertion
unit_scaleDownFloorsAtOne = healthPoints (scaleHealth 0.3 (health 1)) @?= 1

-- | Factor identidad (1.0) deja la salud intacta: @ceiling (1.0 * n) = n@.
unit_identityKeepsHealth :: Assertion
unit_identityKeepsHealth = healthPoints (scaleHealth 1.0 (health 2)) @?= 2
