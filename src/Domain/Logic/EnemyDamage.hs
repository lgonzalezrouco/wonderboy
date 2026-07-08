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

-- Hace parpadear al enemigo en un golpe que sobrevive. Omite el parpadeo en un golpe fatal porque está por eliminarse.
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
