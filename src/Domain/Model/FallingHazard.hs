module Domain.Model.FallingHazard (
  FallingHazardPhase (..),
  FallingHazard (..),
  fallingHazardAabb,
  fallingHazardIsActive,
  spawnFallingHazard,
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Aabb (Aabb, aabbFromBottomCenter)
import Domain.ValueObjects.Frames (Frames)
import Domain.ValueObjects.Position (Position)

data FallingHazardPhase
  = HazardFalling
  | HazardWaiting Frames -- en pausa en el origen, Frames hasta la próxima caída
  | HazardDone -- terminado, eliminado del mundo
  deriving (Eq, Show, Generic)

data FallingHazard = FallingHazard
  { fallingHazardId :: Int
  , fallingHazardSpawnPos :: Position
  -- ^ Origen fijo al que vuelve cuando hace loop.
  , fallingHazardPos :: Position
  , fallingHazardWidth :: Float
  , fallingHazardHeight :: Float
  , fallingHazardFallSpeed :: Float
  -- ^ Velocidad de caída en px/s.
  , fallingHazardLoopDelay :: Maybe Frames
  -- ^ Frames a esperar antes de volver a caer. Nothing significa que cae una sola vez.
  , fallingHazardPhase :: FallingHazardPhase
  }
  deriving (Eq, Show, Generic)

fallingHazardAabb :: FallingHazard -> Aabb
fallingHazardAabb h =
  aabbFromBottomCenter
    (fallingHazardPos h)
    (fallingHazardWidth h)
    (fallingHazardHeight h)

fallingHazardIsActive :: FallingHazard -> Bool
fallingHazardIsActive h = fallingHazardPhase h /= HazardDone

spawnFallingHazard ::
  Int ->
  Position ->
  Float ->
  Float ->
  Float ->
  Maybe Frames ->
  FallingHazard
spawnFallingHazard hid spawnPos width height fallSpeed loopDelay =
  FallingHazard
    { fallingHazardId = hid
    , fallingHazardSpawnPos = spawnPos
    , fallingHazardPos = spawnPos
    , fallingHazardWidth = width
    , fallingHazardHeight = height
    , fallingHazardFallSpeed = fallSpeed
    , fallingHazardLoopDelay = loopDelay
    , fallingHazardPhase = HazardFalling
    }
