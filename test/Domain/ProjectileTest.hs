-- | Pure throw and projectile tests.
module Domain.ProjectileTest where

import Domain.Fixtures (
  dtFrame,
  floorWorld,
  testCombatParams,
  testParams,
  testPlayerProjectile,
  testThrowParams,
 )
import Domain.Logic.Combat (resolveCombat)
import Domain.Logic.EntityBehaviours (patrolHorizontal)
import Domain.Logic.Projectiles (resolveProjectiles)
import Domain.Model.Enemy (mkEnemy)
import Domain.Model.Player (playerAttackFrames, playerThrowCooldownFrames)
import Domain.Model.Projectile (
  ProjectileMotion (..),
  ProjectileOwner (..),
  projectileLifetime,
  projectileMotion,
  projectileOwner,
 )
import Domain.Model.World (World (..))
import Domain.ValueObjects.CombatParams (cpAttackDuration)
import Domain.ValueObjects.Frames (frames, tickFrames)
import Domain.ValueObjects.Input (Input (..), noInput)
import Domain.ValueObjects.Position (position)
import Domain.ValueObjects.ThrowParams (tpCooldown, tpLifetime)
import Domain.ValueObjects.Velocity (velocity)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))

stepProjectiles :: Input -> World -> World
stepProjectiles = resolveProjectiles testThrowParams testCombatParams testParams dtFrame

unit_throwSpawnsProjectile :: Assertion
unit_throwSpawnsProjectile =
  let w' = stepProjectiles (noInput{inputThrow = True}) floorWorld
   in case worldProjectiles w' of
        [proj] -> do
          projectileOwner proj @?= PlayerProjectile
          projectileMotion proj @?= Ballistic
          projectileLifetime proj @?= tickFrames (tpLifetime testThrowParams)
        _ -> assertFailure "expected exactly one projectile"

unit_throwBlockedDuringFlight :: Assertion
unit_throwBlockedDuringFlight =
  let flying = testPlayerProjectile 1 (position 40 40) (velocity 100 100) (frames 60)
      w =
        floorWorld
          { worldProjectiles = [flying]
          , worldNextProjectileId = 2
          , worldFallingHazards = []
          , worldCrumblingPlatforms = []
          }
      w' = stepProjectiles (noInput{inputThrow = True}) w
   in length (worldProjectiles w') @?= 1

unit_throwBlockedOnCooldown :: Assertion
unit_throwBlockedOnCooldown =
  let w =
        floorWorld
          { worldPlayer =
              (worldPlayer floorWorld){playerThrowCooldownFrames = frames 10}
          }
      w' = stepProjectiles (noInput{inputThrow = True}) w
   in worldProjectiles w' @?= []

unit_projectileHitRemovesEnemy :: Assertion
unit_projectileHitRemovesEnemy =
  let enemy = mkEnemy 1 (position 50 8) (patrolHorizontal 10 (frames 10))
      proj = testPlayerProjectile 1 (position 50 20) (velocity 0 0) (frames 60)
      w =
        floorWorld
          { worldEnemies = [enemy]
          , worldProjectiles = [proj]
          }
      w' = stepProjectiles noInput w
   in do
        worldEnemies w' @?= []
        worldProjectiles w' @?= []

unit_projectileDespawnsOnPlatform :: Assertion
unit_projectileDespawnsOnPlatform =
  let proj = testPlayerProjectile 1 (position 0 12) (velocity 0 (-500)) (frames 60)
      w = floorWorld{worldProjectiles = [proj]}
      w' = stepProjectiles noInput w
   in worldProjectiles w' @?= []

unit_projectileLifetimeTimeout :: Assertion
unit_projectileLifetimeTimeout =
  let proj = testPlayerProjectile 1 (position 0 200) (velocity 0 0) (frames 1)
      w = floorWorld{worldProjectiles = [proj]}
      w' = stepProjectiles noInput w
   in worldProjectiles w' @?= []

unit_cooldownAfterDespawn :: Assertion
unit_cooldownAfterDespawn =
  let proj = testPlayerProjectile 1 (position 0 200) (velocity 0 0) (frames 1)
      w = floorWorld{worldProjectiles = [proj]}
      w' = stepProjectiles noInput w
   in playerThrowCooldownFrames (worldPlayer w') @?= tpCooldown testThrowParams

unit_throwCoexistsWithMelee :: Assertion
unit_throwCoexistsWithMelee =
  let input = noInput{inputThrow = True, inputAttack = True}
      wCombat = resolveCombat testCombatParams input floorWorld
      w' = stepProjectiles input wCombat
   in do
        length (worldProjectiles w') @?= 1
        playerAttackFrames (worldPlayer w') @?= cpAttackDuration testCombatParams
