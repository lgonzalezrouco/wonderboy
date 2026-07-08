-- | Daño del jugador a enemigos y destello visual de supervivencia.
module Domain.Logic.EnemyDamage (
  applyPlayerDamageToEnemy,
  tickEnemyHurtFrames,
)
where

import Domain.Model.Enemy (Enemy (..))
import Domain.ValueObjects.CombatParams (CombatParams (..))
import Domain.ValueObjects.Damage (Damage)
import Domain.ValueObjects.Frames (hasFramesLeft, tickFrames)
import Domain.ValueObjects.Health (isDepleted, reduceHealth)

applyPlayerDamageToEnemy :: CombatParams -> Damage -> Enemy -> Enemy
applyPlayerDamageToEnemy cp dmg e =
  let health' = reduceHealth dmg (enemyHealth e)
   in e
        { enemyHealth = health'
        , enemyHurtFrames =
            if isDepleted health'
              then enemyHurtFrames e
              else cpEnemyHurtFlashDuration cp
        }

tickEnemyHurtFrames :: Enemy -> Enemy
tickEnemyHurtFrames e
  | hasFramesLeft (enemyHurtFrames e) =
      e{enemyHurtFrames = tickFrames (enemyHurtFrames e)}
  | otherwise =
      e
