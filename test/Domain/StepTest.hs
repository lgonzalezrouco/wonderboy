module Domain.StepTest (tests) where

import Domain.Logic.Step (step)
import Domain.Model.Player (
  Player (..),
  playerOnGround,
  playerPos,
  playerVel,
  spawnPlayer,
 )
import Domain.Model.World (World (..), initialWorld)
import Domain.ValueObjects.DeltaTime (DeltaTime, deltaTime)
import Domain.ValueObjects.Input (Input (..), noInput)
import Domain.ValueObjects.PhysicsParams (PhysicsParams (..), physicsParams)
import Domain.ValueObjects.Position (posX, position)
import Domain.ValueObjects.Velocity (velY, velocity)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

testParams :: PhysicsParams
testParams = physicsParams 980 200 400

dtFrame :: DeltaTime
dtFrame = deltaTime 0.016

tests :: TestTree
tests =
  testGroup
    "Domain.Logic.Step"
    [ testCase "stepZeroIsIdentity at spawn" testStepZeroIsIdentitySpawn
    , testCase "stepZeroIsIdentity with horizontal velocity" testStepZeroIsIdentityMoving
    , testCase "gravityMonotone in air" testGravityMonotone
    , testCase "jumpImpulse matches ppJumpSpeed" testJumpImpulse
    ]

testStepZeroIsIdentitySpawn :: Assertion
testStepZeroIsIdentitySpawn =
  step testParams (deltaTime 0) noInput initialWorld @?= initialWorld

testStepZeroIsIdentityMoving :: Assertion
testStepZeroIsIdentityMoving =
  let p0 = spawnPlayer (position 10 50)
      moving =
        initialWorld
          { worldPlayer =
              p0
                { playerVel = velocity 150 (-30)
                , playerOnGround = False
                }
          }
   in step testParams (deltaTime 0) noInput moving @?= moving

testGravityMonotone :: Assertion
testGravityMonotone =
  let w1 = step testParams dtFrame noInput initialWorld
      vy0 = velY (playerVel (worldPlayer initialWorld))
      vy1 = velY (playerVel (worldPlayer w1))
   in vy1 < vy0 @?= True

testJumpImpulse :: Assertion
testJumpImpulse = do
  let wGround = fallUntilGround 200 initialWorld
  assertBool "player should be on ground" (playerOnGround (worldPlayer wGround))
  posX (playerPos (worldPlayer wGround)) @?= 0
  let wJump =
        step testParams dtFrame (noInput{inputJump = True}) wGround
      vy = velY (playerVel (worldPlayer wJump))
  vy @?= ppJumpSpeed testParams

fallUntilGround :: Int -> World -> World
fallUntilGround 0 w = w
fallUntilGround n w
  | playerOnGround (worldPlayer w) = w
  | otherwise = fallUntilGround (n - 1) (step testParams dtFrame noInput w)
