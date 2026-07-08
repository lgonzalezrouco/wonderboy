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

newtype Health = Health Int
  deriving (Eq, Ord, Show, Generic)

health :: Int -> Health
health n = Health (max 0 n)

healthPoints :: Health -> Int
healthPoints (Health n) = n

isDepleted :: Health -> Bool
isDepleted (Health n) = n <= 0

reduceHealth :: Damage -> Health -> Health
reduceHealth d (Health n) = health (n - damagePoints d)

-- | Escala la salud por un factor, redondeando al más cercano, con un piso de 1 HP para que nada aparezca ya muerto.
scaleHealth :: Float -> Health -> Health
scaleHealth factor h =
  health (max 1 (round (fromIntegral (healthPoints h) * factor)))
