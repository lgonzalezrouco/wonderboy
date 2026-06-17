{- | Cantidad de daño infligido a una entidad.

Value object con invariante: el daño nunca es negativo. El constructor de datos
no se exporta; se fuerza el smart constructor 'damage', igual que 'DeltaTime'.
-}
module Domain.ValueObjects.Damage (
  Damage,
  damage,
  damagePoints,
)
where

import GHC.Generics (Generic)

-- | Daño en puntos de salud (>= 0).
newtype Damage = Damage Int
  deriving (Eq, Ord, Show, Generic)

-- | Construye un 'Damage', saturando en 0 (no hay daño negativo).
damage :: Int -> Damage
damage n = Damage (max 0 n)

-- | Puntos de daño como 'Int'.
damagePoints :: Damage -> Int
damagePoints (Damage n) = n
