{- | Combate cuerpo a cuerpo y contacto enemigo (puro).

Orquestación por frame en @UseCases.UpdateGame@: tras física, antes de
out-of-bounds y muerte.
-}
module Domain.Logic.Combat (
  resolveCombat,
  meleeHitbox,
)
where

import Domain.Logic.PlayerLife (applyDamage)
import Domain.Model.Enemy (Enemy (..), enemyAabb)
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
  aabbMinX,
  aabbOverlaps,
 )
import Domain.ValueObjects.CombatParams (CombatParams (..))
import Domain.ValueObjects.Facing (Facing (..), facingTowardHorizontal)
import Domain.ValueObjects.Frames (hasFramesLeft, tickFrames)
import Domain.ValueObjects.Health (isDepleted, reduceHealth)
import Domain.ValueObjects.Input (Input (..), inputHorizontalSign)

-- | Facing, ataque, frames de invencibilidad, melee y contacto en un solo paso puro.
resolveCombat :: CombatParams -> Input -> World -> World
resolveCombat cp input w =
  let p0 = worldPlayer w
      p1 = updateFacing input p0
      attackStarted = inputAttack input && not (hasFramesLeft (playerAttackFrames p1))
      p2 = startAttack cp input p1
      w1 = w{worldPlayer = p2}
      w2 = resolveMelee cp w1
      p3 = decrementAttack attackStarted (worldPlayer w2)
      w3 = w2{worldPlayer = p3}
      w4 = resolveContact cp w3
      p4 = tickInvincibility (worldPlayer w4)
   in w4{worldPlayer = p4}

{- | Orienta al jugador según la intención horizontal del frame.

La dirección queda __fija mientras hay un ataque activo__ (@playerAttackFrames > 0@):
como 'resolveCombat' llama a 'updateFacing' antes de iniciar el ataque, en el frame de
inicio el contador aún es 0 y el facing se fija con el input de ese frame; durante el
resto de la ventana no se reorienta, de modo que el swing no se "da vuelta" a mitad.
-}
updateFacing :: Input -> Player -> Player
updateFacing inp p
  | hasFramesLeft (playerAttackFrames p) = p
  | otherwise =
      p{playerFacing = facingTowardHorizontal (playerFacing p) (inputHorizontalSign inp)}

startAttack :: CombatParams -> Input -> Player -> Player
startAttack cp inp p
  | inputAttack inp
  , not (hasFramesLeft (playerAttackFrames p)) =
      p{playerAttackFrames = cpAttackDuration cp}
  | otherwise =
      p

decrementAttack :: Bool -> Player -> Player
decrementAttack attackStarted p
  | attackStarted = p
  | hasFramesLeft (playerAttackFrames p) =
      p{playerAttackFrames = tickFrames (playerAttackFrames p)}
  | otherwise =
      p

tickInvincibility :: Player -> Player
tickInvincibility p
  | hasFramesLeft (playerInvincibilityFrames p) =
      p{playerInvincibilityFrames = tickFrames (playerInvincibilityFrames p)}
  | otherwise =
      p

resolveMelee :: CombatParams -> World -> World
resolveMelee cp w =
  let p = worldPlayer w
   in if playerAttackFrames p /= cpAttackDuration cp
        then w
        else
          let body = playerAabb p
              hitbox = meleeHitbox cp body (playerFacing p)
              hitsEnemy e = aabbOverlaps hitbox (enemyAabb e) || aabbOverlaps body (enemyAabb e)
              applyMeleeHit e
                | hitsEnemy e = e{enemyHealth = reduceHealth (cpMeleeDamage cp) (enemyHealth e)}
                | otherwise = e
           in w{worldEnemies = filter (not . isDepleted . enemyHealth) (map applyMeleeHit (worldEnemies w))}

{- | Caja de alcance del melee, extendida desde la caja del jugador hacia su facing.

Se construye por /update de record/ sobre @body@ para preservar los bordes verticales
(@aabbMinY@/@aabbMaxY@) del jugador y expresar únicamente la diferencia horizontal.
-}
meleeHitbox :: CombatParams -> Aabb -> Facing -> Aabb
meleeHitbox cp body facing =
  let reach = cpMeleeReach cp
   in case facing of
        FacingRight -> body{aabbMinX = aabbMaxX body, aabbMaxX = aabbMaxX body + reach}
        FacingLeft -> body{aabbMinX = aabbMinX body - reach, aabbMaxX = aabbMinX body}

resolveContact :: CombatParams -> World -> World
resolveContact cp w
  | hasFramesLeft (playerInvincibilityFrames (worldPlayer w)) = w
  | any (isDamagingContact (worldPlayer w)) (worldEnemies w) =
      let p = worldPlayer w
          p' =
            applyDamage
              (cpContactDamage cp)
              p
                { playerInvincibilityFrames = cpInvincibilityDuration cp
                }
       in w{worldPlayer = p'}
  | otherwise = w

-- | Cualquier solape jugador–enemigo daña (sin excepción de pisotón).
isDamagingContact :: Player -> Enemy -> Bool
isDamagingContact p e =
  aabbOverlaps (playerAabb p) (enemyAabb e)
