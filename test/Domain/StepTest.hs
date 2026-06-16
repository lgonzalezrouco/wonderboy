module Domain.StepTest where

import Domain.Fixtures (
  ceilingPlatform,
  dtFrame,
  testParams,
  wallPlatform,
  worldGrounded,
  worldWithCeiling,
  worldWithWall,
 )
import Domain.Logic.Step (step)
import Domain.Model.Platform (platformAabb)
import Domain.Model.Player (
  playerAabb,
  playerOnGround,
  playerPos,
  playerVel,
  playerWidth,
  spawnPlayer,
 )
import Domain.Model.World (World (..), defaultMaxHealth, initialWorld)
import Domain.ValueObjects.Aabb (aabbMaxY, aabbMinX, aabbMinY)
import Domain.ValueObjects.DeltaTime (deltaTime)
import Domain.ValueObjects.Input (Input (..), noInput)
import Domain.ValueObjects.PhysicsParams (ppJumpSpeed, ppMoveSpeed)
import Domain.ValueObjects.Position (posX, position)
import Domain.ValueObjects.Velocity (velX, velY, velocity)
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))

unit_stepZeroIsIdentityAtSpawn :: Assertion
unit_stepZeroIsIdentityAtSpawn =
  step testParams (deltaTime 0) noInput initialWorld @?= initialWorld

unit_stepZeroIsIdentityMoving :: Assertion
unit_stepZeroIsIdentityMoving =
  let p0 = spawnPlayer defaultMaxHealth (position 10 50)
      moving =
        initialWorld
          { worldPlayer =
              p0
                { playerVel = velocity 150 (-30)
                , playerOnGround = False
                }
          }
   in step testParams (deltaTime 0) noInput moving @?= moving

unit_gravityMonotoneInAir :: Assertion
unit_gravityMonotoneInAir = do
  let w1 = step testParams dtFrame noInput initialWorld
      vy0 = velY (playerVel (worldPlayer initialWorld))
      vy1 = velY (playerVel (worldPlayer w1))
  vy1 < vy0 @?= True

unit_landingSetsOnGroundAndZeroesVy :: Assertion
unit_landingSetsOnGroundAndZeroesVy = do
  w <- worldGrounded
  let p = worldPlayer w
  assertBool "player should be on ground" (playerOnGround p)
  velY (playerVel p) @?= 0

unit_jumpImpulseMatchesPpJumpSpeed :: Assertion
unit_jumpImpulseMatchesPpJumpSpeed = do
  wGround <- worldGrounded
  posX (playerPos (worldPlayer wGround)) @?= 0
  let wJump = step testParams dtFrame (noInput{inputJump = True}) wGround
      vy = velY (playerVel (worldPlayer wJump))
  vy @?= ppJumpSpeed testParams

unit_jumpGatingInAir :: Assertion
unit_jumpGatingInAir = do
  let wAir =
        initialWorld
          { worldPlayer =
              (spawnPlayer defaultMaxHealth (position 0 80))
                { playerVel = velocity 0 (-100)
                , playerOnGround = False
                }
          }
      wNoJump = step testParams dtFrame noInput wAir
      wJump = step testParams dtFrame (noInput{inputJump = True}) wAir
  worldPlayer wJump @?= worldPlayer wNoJump

unit_horizontalInputLeft :: Assertion
unit_horizontalInputLeft = do
  let w = step testParams dtFrame (noInput{inputLeft = True}) initialWorld
  velX (playerVel (worldPlayer w)) @?= (-ppMoveSpeed testParams)

unit_horizontalInputRight :: Assertion
unit_horizontalInputRight = do
  let w = step testParams dtFrame (noInput{inputRight = True}) initialWorld
  velX (playerVel (worldPlayer w)) @?= ppMoveSpeed testParams

unit_ceilingBumpZeroesVy :: Assertion
unit_ceilingBumpZeroesVy = do
  let w0 = worldWithCeiling
      vyBefore = velY (playerVel (worldPlayer w0))
  assertBool "setup: player moving upward" (vyBefore > 0)
  let w1 = step testParams dtFrame noInput w0
      p1 = worldPlayer w1
      solid = platformAabb ceilingPlatform
  velY (playerVel p1) @?= 0
  assertBool
    "player stays below ceiling underside"
    (aabbMaxY (playerAabb p1) <= aabbMinY solid + 1e-3)

unit_wallBlockNoPenetration :: Assertion
unit_wallBlockNoPenetration = do
  let w1 = step testParams dtFrame (noInput{inputRight = True}) worldWithWall
      p1 = worldPlayer w1
      wallFace = aabbMinX (platformAabb wallPlatform)
      maxFootX = wallFace - playerWidth / 2
  assertBool
    "player does not pass wall inner face"
    (posX (playerPos p1) <= maxFootX + 1e-3)
