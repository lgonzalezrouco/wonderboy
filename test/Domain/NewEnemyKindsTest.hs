-- | Archer and Bat 2D chase.
module Domain.NewEnemyKindsTest where

import Domain.Fixtures (
  dtFrame,
  enemyFrom,
  floorWorld,
  runBehaviourN,
  testCombatParams,
  testEnemyProjectile,
  testParams,
  testThrowParams,
  worldWithEnemyAt,
 )
import Domain.Logic.Projectiles (resolveProjectiles)
import Domain.Model.Enemy (enemyVel)
import Domain.Model.EnemyKind (EnemyKind (..))
import Domain.Model.Player (playerHealth, spawnPlayer)
import Domain.Model.Projectile (ProjectileOwner (..), projectileOwner)
import Domain.Model.World (World (..))
import Domain.ValueObjects.Frames (frames)
import Domain.ValueObjects.Health (health)
import Domain.ValueObjects.Input (noInput)
import Domain.ValueObjects.Position (position)
import Domain.ValueObjects.Velocity (velY, velocity)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))

unit_batChaseUsesVerticalVelocity :: Assertion
unit_batChaseUsesVerticalVelocity =
  let w0 = worldWithEnemyAt BatKind (position 80 80) (position 0 8)
      w1 = runBehaviourN 2 w0
      e = enemyFrom w1
   in assertBool "bat chase homes vertically toward player" (velY (enemyVel e) /= 0)

unit_archerFiresInRange :: Assertion
unit_archerFiresInRange =
  let w0 = worldWithEnemyAt ArcherKind (position 50 8) (position 0 8)
      w1 = runBehaviourN 4 w0
   in case worldProjectiles w1 of
        proj : _ -> projectileOwner proj @?= EnemyProjectile
        [] -> assertFailure "expected archer to spawn enemy projectile"

unit_archerSilentOutOfRange :: Assertion
unit_archerSilentOutOfRange =
  let w0 = worldWithEnemyAt ArcherKind (position 50 8) (position (-300) 8)
      w1 = runBehaviourN 4 w0
   in worldProjectiles w1 @?= []

unit_enemyProjectileDamagesPlayer :: Assertion
unit_enemyProjectileDamagesPlayer =
  let w0 =
        floorWorld
          { worldPlayer = spawnPlayer (health 3) (position 40 8)
          , worldProjectiles =
              [testEnemyProjectile 1 (position 40 8) (velocity 0 0) (frames 30)]
          }
      w1 =
        resolveProjectiles
          testThrowParams
          testCombatParams
          testParams
          dtFrame
          noInput
          w0
   in playerHealth (worldPlayer w1) @?= health 2
