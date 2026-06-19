-- | Pure moving platform motion, carry, and landing tests.
module Domain.MovingPlatformTest where

import Domain.Logic.MovingPlatforms (
  advanceMovingPlatforms,
  mpaDeltaX,
  mpaDeltaY,
  mpaPlatform,
 )
import Domain.Logic.Step (step)
import Domain.Model.ExitZone (defaultExitZone)
import Domain.Model.MovingPlatform (MovingPlatform (..), mkMovingPlatform, movingPlatformPos)
import Domain.Model.Platform (Platform, platform)
import Domain.Model.Player (
  Player (..),
  playerOnGround,
  playerPos,
  spawnPlayer,
 )
import Domain.Model.World (World (..), defaultMaxHealth)
import Domain.ValueObjects.DeltaTime (DeltaTime, deltaTime)
import Domain.ValueObjects.Input (Input (..), noInput)
import Domain.ValueObjects.PhysicsParams (PhysicsParams (..), physicsParams)
import Domain.ValueObjects.Position (Position, posX, posY, position)
import Domain.ValueObjects.Score (score)
import Domain.ValueObjects.Velocity (velX, velocity)
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))

testParams :: PhysicsParams
testParams = physicsParams 980 200 400

dtFrame :: DeltaTime
dtFrame = deltaTime 0.016

mustMovingPlatform :: Maybe MovingPlatform -> MovingPlatform
mustMovingPlatform (Just mp) = mp
mustMovingPlatform Nothing = error "mustMovingPlatform: invalid fixture"

horizontalShuttle :: MovingPlatform
horizontalShuttle =
  mustMovingPlatform $
    mkMovingPlatform
      1
      (position 30 36)
      48
      8
      (position 30 36)
      (position 90 36)
      35
      True

-- | Pies sobre el tramo superior del shuttle horizontal (top y = 44).
onHorizontalShuttleTop :: Position
onHorizontalShuttleTop = position 60 44

verticalShuttle :: MovingPlatform
verticalShuttle =
  mustMovingPlatform $
    mkMovingPlatform
      2
      (position 0 24)
      48
      8
      (position 0 24)
      (position 0 80)
      30
      True

floorPlat :: Platform
floorPlat = platform (position (-200) 0) 400 8

playerOnShuttle :: Position -> Player
playerOnShuttle pos =
  (spawnPlayer defaultMaxHealth pos){playerOnGround = True, playerVel = velocity 0 0}

worldWithShuttle :: MovingPlatform -> Player -> World
worldWithShuttle mp p =
  World
    { worldPlayer = p
    , worldEnemies = []
    , worldPlatforms = [floorPlat]
    , worldMovingPlatforms = [mp]
    , worldSpawnPoint = playerPos p
    , worldPickups = []
    , worldMinScore = score 0
    , worldExit = defaultExitZone
    , worldProjectiles = []
    , worldNextProjectileId = 1
    , worldFallingHazards = []
    }

unit_mkMovingPlatformRejectsInvalid :: Assertion
unit_mkMovingPlatformRejectsInvalid = do
  mkMovingPlatform 1 (position 0 0) 0 8 (position 0 0) (position 10 0) 10 True @?= Nothing
  mkMovingPlatform 1 (position 0 0) 10 8 (position 0 0) (position 10 0) 0 True @?= Nothing
  mkMovingPlatform 1 (position 0 0) 10 8 (position 0 0) (position 10 10) 10 True @?= Nothing
  mkMovingPlatform 1 (position 5 5) 10 8 (position 0 0) (position 10 0) 10 True @?= Nothing

unit_pingPongReversesAtEndpoint :: Assertion
unit_pingPongReversesAtEndpoint =
  let advances = advanceMovingPlatforms (deltaTime 10) [horizontalShuttle]
   in case advances of
        (adv : _) ->
          let mp = mpaPlatform adv
           in do
                posX (movingPlatformPos mp) @?= 90
                movingPlatformTowardB mp @?= False
        [] -> error "expected one advance result"

unit_horizontalCarryOnGround :: Assertion
unit_horizontalCarryOnGround =
  let pos = onHorizontalShuttleTop
      w0 = worldWithShuttle horizontalShuttle (playerOnShuttle pos)
      expectedDx =
        case advanceMovingPlatforms dtFrame [horizontalShuttle] of
          (adv : _) -> mpaDeltaX adv
          [] -> 0
      w1 = step testParams dtFrame noInput w0
      dx = posX (playerPos (worldPlayer w1)) - posX (playerPos (worldPlayer w0))
   in assertBool "player horizontal delta matches platform" (abs (dx - expectedDx) <= 1e-3)

unit_verticalCarryDownOnGround :: Assertion
unit_verticalCarryDownOnGround =
  let mp =
        verticalShuttle
          { movingPlatformPos = position 0 80
          , movingPlatformTowardB = False
          }
      pos = position 0 88
      w0 = worldWithShuttle mp (playerOnShuttle pos)
      expectedDy =
        case advanceMovingPlatforms dtFrame [mp] of
          (adv : _) -> mpaDeltaY adv
          [] -> 0
      w1 = step testParams dtFrame noInput w0
      dy = posY (playerPos (worldPlayer w1)) - posY (playerPos (worldPlayer w0))
   in assertBool "player vertical delta matches descending shuttle" (abs (dy - expectedDy) <= 1e-3)

unit_jumpOffDoesNotInheritPlatformVelocity :: Assertion
unit_jumpOffDoesNotInheritPlatformVelocity =
  let pos = onHorizontalShuttleTop
      w0 =
        worldWithShuttle
          horizontalShuttle
          (playerOnShuttle pos)
            { playerVel = velocity 0 400
            }
      w1 = step testParams dtFrame (noInput{inputJump = True}) w0
   in velX (playerVel (worldPlayer w1)) @?= 0

unit_noCarryInAir :: Assertion
unit_noCarryInAir =
  let pos = position 0 80
      p =
        (spawnPlayer defaultMaxHealth pos)
          { playerOnGround = False
          , playerVel = velocity 0 100
          }
      w0 = worldWithShuttle horizontalShuttle p
      w1 = step testParams dtFrame noInput w0
   in posX (playerPos (worldPlayer w1)) @?= posX pos

unit_verticalCarryOnGround :: Assertion
unit_verticalCarryOnGround =
  let pos = position 24 32
      w0 = worldWithShuttle verticalShuttle (playerOnShuttle pos)
      w1 = step testParams dtFrame noInput w0
      dy = posY (playerPos (worldPlayer w1)) - posY (playerPos (worldPlayer w0))
   in assertBool "player moves vertically with shuttle" (dy > 0.01)

unit_jumpIntoSideDoesNotTeleport :: Assertion
unit_jumpIntoSideDoesNotTeleport =
  let w0 =
        World
          { worldPlayer =
              (spawnPlayer defaultMaxHealth (position 20 8))
                { playerOnGround = True
                , playerVel = velocity 0 0
                }
          , worldEnemies = []
          , worldPlatforms = [floorPlat]
          , worldMovingPlatforms = [horizontalShuttle]
          , worldSpawnPoint = position 20 8
          , worldPickups = []
          , worldMinScore = score 0
          , worldExit = defaultExitZone
          , worldProjectiles = []
          , worldNextProjectileId = 1
          , worldFallingHazards = []
          }
      w1 = step testParams dtFrame (noInput{inputRight = True, inputJump = True}) w0
      px = posX (playerPos (worldPlayer w1))
   in do
        assertBool "did not tunnel past platform right edge" (px < 78)
        assertBool "did not teleport left while hitting platform side" (px > 0)

unit_sideContactDoesNotCarry :: Assertion
unit_sideContactDoesNotCarry =
  let pos = position 10 8
      p =
        (spawnPlayer defaultMaxHealth pos)
          { playerOnGround = True
          , playerVel = velocity 0 0
          }
      w0 = worldWithShuttle horizontalShuttle p
      w1 = step testParams dtFrame noInput w0
      dx = posX (playerPos (worldPlayer w1)) - posX pos
   in assertBool "side contact does not apply platform carry" (abs dx < 0.5)

unit_ceilingBumpUnderMovingPlatformDoesNotNudgeSideways :: Assertion
unit_ceilingBumpUnderMovingPlatformDoesNotNudgeSideways =
  let pos = position 60 23
      p =
        (spawnPlayer defaultMaxHealth pos)
          { playerOnGround = False
          , playerVel = velocity 0 400
          }
      w0 =
        World
          { worldPlayer = p
          , worldEnemies = []
          , worldPlatforms = []
          , worldMovingPlatforms =
              [ horizontalShuttle
                  { movingPlatformPos = position 30 72
                  , movingPlatformEndA = position 30 72
                  , movingPlatformEndB = position 90 72
                  }
              ]
          , worldSpawnPoint = pos
          , worldPickups = []
          , worldMinScore = score 0
          , worldExit = defaultExitZone
          , worldProjectiles = []
          , worldNextProjectileId = 1
          , worldFallingHazards = []
          }
      w1 = step testParams dtFrame noInput w0
      p1 = worldPlayer w1
   in do
        posX (playerPos p1) @?= posX pos
        assertBool "player stays below moving platform underside" (posY (playerPos p1) < 30)

unit_landingOnMovingPlatformSetsOnGround :: Assertion
unit_landingOnMovingPlatformSetsOnGround =
  let mp = horizontalShuttle
      w0 =
        World
          { worldPlayer =
              (spawnPlayer defaultMaxHealth (position 60 56))
                { playerVel = velocity 0 (-200)
                , playerOnGround = False
                }
          , worldEnemies = []
          , worldPlatforms = []
          , worldMovingPlatforms = [mp]
          , worldSpawnPoint = position 60 56
          , worldPickups = []
          , worldMinScore = score 0
          , worldExit = defaultExitZone
          , worldProjectiles = []
          , worldNextProjectileId = 1
          , worldFallingHazards = []
          }
      w1 = foldl (\w _ -> step testParams dtFrame noInput w) w0 ([1 .. 15] :: [Int])
   in playerOnGround (worldPlayer w1) @?= True
