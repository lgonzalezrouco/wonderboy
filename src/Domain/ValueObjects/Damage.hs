module Domain.ValueObjects.Damage (
  Damage,
  damage,
  damagePoints,
)
where

import GHC.Generics (Generic)

newtype Damage = Damage Int
  deriving (Eq, Ord, Show, Generic)

damage :: Int -> Damage
damage n = Damage (max 0 n)

damagePoints :: Damage -> Int
damagePoints (Damage n) = n
