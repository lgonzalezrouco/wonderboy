{- | Combate cuerpo a cuerpo y contacto enemigo.

Orquestación por frame en @UseCases.UpdateGame@: tras física, antes de
out-of-bounds y muerte.
-}
module Domain.Logic.Combat (
  resolveCombat,
)
where

import Domain.Logic.BossArena (playerMayDamageEnemy)
import Domain.Logic.EnemyDamage (applyPlayerDamageToEnemy, tickEnemyHurtFrames)
import Domain.Logic.MeleeSwing (meleeHitboxWhenImpact)
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
import Domain.ValueObjects.Aabb (aabbOverlaps)
import Domain.ValueObjects.CombatParams (CombatParams (..))
import Domain.ValueObjects.Facing (facingTowardHorizontal)
import Domain.ValueObjects.Frames (hasFramesLeft, tickFrames)
import Domain.ValueObjects.Health (isDepleted)
import Domain.ValueObjects.Input (Input (..), inputHorizontalSign)

resolveCombat :: CombatParams -> Input -> World -> World
resolveCombat cp input w =
  let w0 = w{worldEnemies = map tickEnemyHurtFrames (worldEnemies w)}
      p0 = worldPlayer w0
      p1 = updateFacing input p0
      attackStarted = inputAttack input && not (hasFramesLeft (playerAttackFrames p1))
      p2 = startAttack cp input p1
      w1 = w0{worldPlayer = p2}
      w2 = resolveMelee cp w1
      p3 = decrementAttack attackStarted (worldPlayer w2)
      w3 = w2{worldPlayer = p3}
      w4 = resolveContact cp w3
      p4 = tickInvincibility (worldPlayer w4)
   in w4{worldPlayer = p4}

{- | Orienta al jugador según la intención horizontal del frame.

La dirección queda __fija mientras hay un ataque activo__ para que el swing no se "dé
vuelta" a mitad; en el frame de inicio el contador aún es 0, así que el facing se fija
con el input de ese frame.
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

{- | Aplica el golpe de melee __solo en el frame de impacto visual__ del swing.

El contador de ataque y 'Domain.Logic.MeleeSwing.isMeleeImpactFrame' fijan un único
frame de daño por press, alineado con el arco de la espada en pantalla.
-}
resolveMelee :: CombatParams -> World -> World
resolveMelee cp w =
  case meleeHitboxWhenImpact cp (worldPlayer w) of
    Nothing -> w
    Just hitbox ->
      let body = playerAabb (worldPlayer w)
          hitsEnemy e = aabbOverlaps hitbox (enemyAabb e) || aabbOverlaps body (enemyAabb e)
          applyMeleeHit e
            | hitsEnemy e
            , playerMayDamageEnemy w e =
                applyPlayerDamageToEnemy cp (cpMeleeDamage cp) e
            | otherwise = e
       in w{worldEnemies = filter (not . isDepleted . enemyHealth) (map applyMeleeHit (worldEnemies w))}

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
