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

healthPoints :: Health -> Int
healthPoints (Health n) = n

-- | 'True' cuando la salud llegó a 0 (entidad derrotada / vida perdida).
isDepleted :: Health -> Bool
isDepleted (Health n) = n <= 0

-- | Aplica un 'Damage' a la salud, saturando en 0.
reduceHealth :: Damage -> Health -> Health
reduceHealth d (Health n) = health (n - damagePoints d)

{- | Escala la salud por un factor (típicamente ya clampeado por 'Multiplier'),
redondeando al entero más cercano, con piso de 1 HP: ningún enemigo nace derrotado.
-}
scaleHealth :: Float -> Health -> Health
scaleHealth factor h =
  health (max 1 (round (fromIntegral (healthPoints h) * factor)))
