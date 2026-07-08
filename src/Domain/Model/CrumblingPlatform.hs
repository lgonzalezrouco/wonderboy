module Domain.Model.CrumblingPlatform (
  CrumblingPlatformPhase (..),
  CrumblingPlatform (..),
  mkCrumblingPlatform,
  spawnCrumblingPlatform,
  crumbleCountdownFrames,
  crumbleFallSpeed,
  crumblingPlatformAabb,
  crumblingPlatformAsPlatform,
  crumblingPlatformIsAnchored,
  crumblingPlatformSolidForPlayer,
)
where

import GHC.Generics (Generic)

import Domain.Model.Platform (Platform, platform)
import Domain.ValueObjects.Aabb (Aabb, aabbFromBottomLeft)
import Domain.ValueObjects.Frames (Frames, frames)
import Domain.ValueObjects.Position (Position)

data CrumblingPlatformPhase
  = CrumbleIntact
  | CrumbleCountingDown Frames -- el jugador la pisó, Frames que quedan antes de caer
  | CrumbleFalling
  deriving (Eq, Show, Generic)

data CrumblingPlatform = CrumblingPlatform
  { crumblingPlatformId :: Int
  , crumblingPlatformPos :: Position
  , crumblingPlatformWidth :: Float
  , crumblingPlatformHeight :: Float
  , crumblingPlatformPhase :: CrumblingPlatformPhase
  }
  deriving (Eq, Show, Generic)

crumbleCountdownFrames :: Frames
crumbleCountdownFrames = frames 15

-- | Velocidad de caída una vez que la plataforma empieza a caer, en px/s.
crumbleFallSpeed :: Float
crumbleFallSpeed = 200

spawnCrumblingPlatform ::
  Int ->
  Position ->
  Float ->
  Float ->
  CrumblingPlatform
spawnCrumblingPlatform pid pos width height =
  CrumblingPlatform
    { crumblingPlatformId = pid
    , crumblingPlatformPos = pos
    , crumblingPlatformWidth = width
    , crumblingPlatformHeight = height
    , crumblingPlatformPhase = CrumbleIntact
    }

mkCrumblingPlatform ::
  Int ->
  Position ->
  Float ->
  Float ->
  Maybe CrumblingPlatform
mkCrumblingPlatform pid pos width height
  | pid > 0 && width > 0 && height > 0 =
      Just (spawnCrumblingPlatform pid pos width height)
  | otherwise = Nothing

crumblingPlatformAsPlatform :: CrumblingPlatform -> Platform
crumblingPlatformAsPlatform cp =
  platform
    (crumblingPlatformPos cp)
    (crumblingPlatformWidth cp)
    (crumblingPlatformHeight cp)

crumblingPlatformAabb :: CrumblingPlatform -> Aabb
crumblingPlatformAabb cp =
  aabbFromBottomLeft
    (crumblingPlatformPos cp)
    (crumblingPlatformWidth cp)
    (crumblingPlatformHeight cp)

crumblingPlatformSolidForPlayer :: CrumblingPlatform -> Bool
crumblingPlatformSolidForPlayer cp = case crumblingPlatformPhase cp of
  CrumbleIntact -> True
  CrumbleCountingDown _ -> True
  CrumbleFalling -> False

crumblingPlatformIsAnchored :: CrumblingPlatform -> Bool
crumblingPlatformIsAnchored = crumblingPlatformSolidForPlayer
