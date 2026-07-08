{- | Recolección de pickups por superposición con el jugador.

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
import Domain.ValueObjects.Score (Score)

resolvePickups :: World -> (World, Score)
resolvePickups w =
  let playerBox = playerAabb (worldPlayer w)
      (collected, remaining) =
        partition (aabbOverlaps playerBox . pickupAabb) (worldPickups w)
      delta = foldMap pickupValue collected
   in (w{worldPickups = remaining}, delta)
