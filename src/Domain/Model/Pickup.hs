module Domain.Model.Pickup (
  Pickup (..),
  pickupWidth,
  pickupHeight,
  pickupAabb,
  mkPickup,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Aabb (Aabb, aabbFromBottomCenter)
import Domain.ValueObjects.Position (Position)
import Domain.ValueObjects.Score (Score, score)

data Pickup = Pickup
  { pickupId :: Int
  , pickupPos :: Position
  , pickupValue :: Score
  }
  deriving (Eq, Show, Generic)

pickupWidth :: Float
pickupWidth = 16.0

pickupHeight :: Float
pickupHeight = 16.0

pickupAabb :: Pickup -> Aabb
pickupAabb p =
  aabbFromBottomCenter (pickupPos p) pickupWidth pickupHeight

-- | Construye un pickup. 'Nothing' si el valor es negativo (cero puntos está permitido).
mkPickup :: Int -> Position -> Int -> Maybe Pickup
mkPickup pid pos value
  | value < 0 = Nothing
  | otherwise =
      Just Pickup{pickupId = pid, pickupPos = pos, pickupValue = score value}
