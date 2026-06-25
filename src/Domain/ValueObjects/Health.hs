{- | Salud (hit points) de una entidad en la vida actual.

Value object con invariante: la salud nunca es negativa (satura en 0). El
constructor de datos no se exporta; se usa el smart constructor 'health'. La
reducción por daño vive en 'reduceHealth' en lugar de una instancia 'Num' (restar
dos saludes o multiplicarlas no tiene sentido).
-}
module Domain.ValueObjects.Health (
  Health,
  health,
  healthPoints,
  isDepleted,
  reduceHealth,
  scaleHealth,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Damage (Damage, damagePoints)

-- | Puntos de vida (>= 0).
newtype Health = Health Int
  deriving (Eq, Ord, Show, Generic)

-- | Construye 'Health', saturando en 0.
health :: Int -> Health
health n = Health (max 0 n)

-- | Puntos de vida como 'Int'.
healthPoints :: Health -> Int
healthPoints (Health n) = n

-- | 'True' cuando la salud llegó a 0 (entidad derrotada / vida perdida).
isDepleted :: Health -> Bool
isDepleted (Health n) = n <= 0

-- | Aplica un 'Damage' a la salud, saturando en 0.
reduceHealth :: Damage -> Health -> Health
reduceHealth d (Health n) = health (n - damagePoints d)

{- | Escala la salud por un factor (típicamente ya clampeado por 'Amplifier'),
redondeando hacia arriba ('ceiling') con piso de 1 HP.

Se usa 'ceiling' y no 'round' a propósito: sobre una base chica (los enemigos comunes
arrancan en 1 HP) cualquier factor > 1.0 debe costar al menos +1 golpe. Con 'round' un
@toughness×@ tibio como 1.2 sobre 1 HP volvía a 1 (@round 1.2 = 1@) y la amplificación se
perdía; peor aún, el redondeo banquero de Haskell dejaba @round 2.5 = 2@, así que ni el
valor más alto que sugiere el modelo llegaba a 3 HP. El piso de 1 HP ('max' con 1) sigue
garantizando que ningún enemigo nace derrotado aunque el factor fuera < 1.
-}
scaleHealth :: Float -> Health -> Health
scaleHealth factor h =
  health (max 1 (ceiling (fromIntegral (healthPoints h) * factor)))
