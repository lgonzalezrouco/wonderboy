module Adapters.Gloss.CameraTest where

import Adapters.Gloss.Camera (cameraXForWorld, clampCameraX, worldHorizontalSpan)
import Domain.Model.Platform (platform)
import Domain.Model.Player (spawnPlayer)
import Domain.Model.World (World (..), defaultMaxHealth, initialWorld)
import Domain.ValueObjects.Position (position)
import Test.Tasty.HUnit (Assertion, (@?=))

unit_clampCameraKeepsMiddleTarget :: Assertion
unit_clampCameraKeepsMiddleTarget =
  clampCameraX 100 500 (0, 1000) @?= 500

unit_clampCameraStopsAtLeftEdge :: Assertion
unit_clampCameraStopsAtLeftEdge =
  clampCameraX 100 25 (0, 1000) @?= 100

unit_clampCameraStopsAtRightEdge :: Assertion
unit_clampCameraStopsAtRightEdge =
  clampCameraX 100 975 (0, 1000) @?= 900

unit_clampCameraCentersNarrowWorld :: Assertion
unit_clampCameraCentersNarrowWorld =
  clampCameraX 400 10 (100, 300) @?= 200

unit_worldHorizontalSpanUsesStaticPlatforms :: Assertion
unit_worldHorizontalSpanUsesStaticPlatforms =
  worldHorizontalSpan testWorld @?= Just (-120, 360)

unit_cameraXForWorldFallsBackWithoutPlatforms :: Assertion
unit_cameraXForWorldFallsBackWithoutPlatforms =
  cameraXForWorld worldWithoutPlatforms @?= 42

testWorld :: World
testWorld =
  initialWorld
    { worldPlayer = spawnPlayer defaultMaxHealth (position 0 0)
    , worldPlatforms =
        [ platform (position (-120) 0) 20 100
        , platform (position 40 0) 320 8
        ]
    }

worldWithoutPlatforms :: World
worldWithoutPlatforms =
  initialWorld
    { worldPlayer = spawnPlayer defaultMaxHealth (position 42 0)
    , worldPlatforms = []
    }
