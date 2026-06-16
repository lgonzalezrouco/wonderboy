{- | Combate cuerpo a cuerpo y contacto enemigo (puro).

Orquestación por frame en @UseCases.UpdateGame@: tras física, antes de
out-of-bounds y muerte.
-}
module Domain.Logic.Combat (
  resolveCombat,
)
where

import Data.List (find)

import Domain.Logic.PlayerLife (applyDamage)
import Domain.Model.Enemy (Enemy, enemyAabb)
import Domain.Model.Player (
  Player (..),
  playerAabb,
  playerAttackFrames,
  playerFacing,
  playerInvincibilityFrames,
 )
import Domain.Model.World (World (..))
import Domain.ValueObjects.Aabb (
  Aabb (..),
  aabbMaxX,
  aabbMaxY,
  aabbMinX,
  aabbMinY,
  aabbOverlaps,
 )
import Domain.ValueObjects.CombatParams (CombatParams (..))
import Domain.ValueObjects.Facing (Facing (..))
import Domain.ValueObjects.Input (Input (..))

contactEpsilon :: Float
contactEpsilon = 1e-3

-- | Facing, ataque, i-frames, melee y contacto en un solo paso puro.
resolveCombat :: CombatParams -> Input -> World -> World
resolveCombat cp input w =
  let p0 = worldPlayer w
      p1 = updateFacing input p0
      p2 = tickAttack cp input p1
      w1 = w{worldPlayer = p2}
      w2 = resolveMelee cp w1
      w3 = resolveContact cp w2
      p4 = tickInvincibility (worldPlayer w3)
   in w3{worldPlayer = p4}

updateFacing :: Input -> Player -> Player
updateFacing inp p =
  case (inputLeft inp, inputRight inp) of
    (True, False) -> p{playerFacing = FacingLeft}
    (False, True) -> p{playerFacing = FacingRight}
    _ -> p

tickAttack :: CombatParams -> Input -> Player -> Player
tickAttack cp inp p
  | inputAttack inp
  , playerAttackFrames p == 0 =
      p{playerAttackFrames = cpAttackDuration cp}
  | playerAttackFrames p > 0 =
      p{playerAttackFrames = playerAttackFrames p - 1}
  | otherwise =
      p

tickInvincibility :: Player -> Player
tickInvincibility p
  | playerInvincibilityFrames p > 0 =
      p{playerInvincibilityFrames = playerInvincibilityFrames p - 1}
  | otherwise =
      p

resolveMelee :: CombatParams -> World -> World
resolveMelee cp w =
  let box = meleeAabb cp (worldPlayer w)
   in case box of
        Nothing -> w
        Just hitbox ->
          w
            { worldEnemies =
                filter (not . enemyOverlapsMelee hitbox) (worldEnemies w)
            }
 where
  enemyOverlapsMelee hitbox e = aabbOverlaps hitbox (enemyAabb e)

meleeAabb :: CombatParams -> Player -> Maybe Aabb
meleeAabb cp p
  | playerAttackFrames p <= 0 = Nothing
  | otherwise =
      Just $
        case playerFacing p of
          FacingRight ->
            let body = playerAabb p
                reach = cpMeleeReach cp
             in Aabb
                  { aabbMinX = aabbMaxX body
                  , aabbMinY = aabbMinY body
                  , aabbMaxX = aabbMaxX body + reach
                  , aabbMaxY = aabbMaxY body
                  }
          FacingLeft ->
            let body = playerAabb p
                reach = cpMeleeReach cp
             in Aabb
                  { aabbMinX = aabbMinX body - reach
                  , aabbMinY = aabbMinY body
                  , aabbMaxX = aabbMinX body
                  , aabbMaxY = aabbMaxY body
                  }

resolveContact :: CombatParams -> World -> World
resolveContact cp w
  | playerInvincibilityFrames (worldPlayer w) > 0 = w
  | otherwise =
      case find (isDamagingContact (worldPlayer w)) (worldEnemies w) of
        Nothing -> w
        Just _ ->
          let p = worldPlayer w
              p' =
                applyDamage (cpContactDamage cp) p
                  { playerInvincibilityFrames = cpInvincibilityDuration cp
                  }
           in w{worldPlayer = p'}

isDamagingContact :: Player -> Enemy -> Bool
isDamagingContact p e =
  let playerBox = playerAabb p
      enemyBox = enemyAabb e
   in aabbOverlaps playerBox enemyBox
        && not (isStompSafe playerBox enemyBox)

isStompSafe :: Aabb -> Aabb -> Bool
isStompSafe playerBox enemyBox =
  aabbMinY playerBox >= aabbMaxY enemyBox - contactEpsilon
