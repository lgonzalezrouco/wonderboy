{- | Recolección de pickups por superposición con el jugador (puro).

Orquestación por frame en @UseCases.UpdateGame@: tras combate, antes de
out-of-bounds y muerte.
-}
module Domain.Logic.Pickups (
  resolvePickups,
)
where

import Data.List (partition)

import Domain.Model.Pickup (pickupAabb, pickupValue)
import Domain.Model.Player (playerAabb)
import Domain.Model.World (World (..))
import Domain.ValueObjects.Aabb (aabbOverlaps)

-- | Particiona pickups superpuestos con el jugador; devuelve mundo actualizado y delta de puntos.
resolvePickups :: World -> (World, Int)
resolvePickups w =
  let playerBox = playerAabb (worldPlayer w)
      (collected, remaining) =
        partition (aabbOverlaps playerBox . pickupAabb) (worldPickups w)
      delta = sum (pickupValue <$> collected)
   in (w{worldPickups = remaining}, delta)
