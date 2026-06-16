{- | Modelo de un pickup coleccionable dentro del mundo del juego.

Un pickup es una entidad no sólida: el jugador pasa a través de ella
kinemáticamente y la recoge por superposición de AABB con su hitbox.
-}
module Domain.Model.Pickup (
  -- * Tipo
  Pickup (..),

  -- * Hitbox
  pickupWidth,
  pickupHeight,
  pickupAabb,

  -- * Construcción
  mkPickup,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Aabb (Aabb, aabbFromBottomCenter)
import Domain.ValueObjects.Position (Position)

{- | Estado de un pickup en un frame dado.

'pickupPos' es la posición de los pies (centro inferior), igual que el jugador
y los enemigos. 'pickupValue' son los puntos otorgados al recogerlo.
-}
data Pickup = Pickup
  { pickupId :: Int
  -- ^ Identificador único del pickup en el nivel.
  , pickupPos :: Position
  -- ^ Posición actual (pies, centro inferior).
  , pickupValue :: Int
  -- ^ Puntos al recoger; debe ser ≥ 0 (validado por 'mkPickup').
  }
  deriving (Eq, Show, Generic)

-- | Ancho del hitbox del pickup en píxeles lógicos.
pickupWidth :: Float
pickupWidth = 16.0

-- | Alto del hitbox del pickup en píxeles lógicos.
pickupHeight :: Float
pickupHeight = 16.0

-- | Caja de colisión del pickup: @pickupPos@ es el centro inferior (pies).
pickupAabb :: Pickup -> Aabb
pickupAabb p =
  aabbFromBottomCenter (pickupPos p) pickupWidth pickupHeight

{- | Crea un pickup con identificador, posición y valor de puntos.

Devuelve 'Nothing' cuando @value < 0@; @value = 0@ es válido.
-}
mkPickup :: Int -> Position -> Int -> Maybe Pickup
mkPickup pid pos value
  | value < 0 = Nothing
  | otherwise =
      Just Pickup{pickupId = pid, pickupPos = pos, pickupValue = value}
