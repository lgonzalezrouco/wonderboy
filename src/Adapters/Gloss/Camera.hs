-- | Pure camera helpers for the Gloss adapter.
module Adapters.Gloss.Camera (
  cameraXForWorld,
  clampCameraX,
  worldHorizontalSpan,
)
where

import Data.List (foldl')

import Adapters.Gloss.Config (renderZoom, windowWidth)
import Domain.Model.Platform (platformAabb)
import Domain.Model.Player (playerPos)
import Domain.Model.World (World (..))
import Domain.ValueObjects.Aabb (Aabb, aabbMaxX, aabbMinX)
import Domain.ValueObjects.Position (posX)

-- | Horizontal camera anchor for the current world, clamped to authored level bounds.
cameraXForWorld :: World -> Float
cameraXForWorld world =
  case worldHorizontalSpan world of
    Nothing -> targetX
    Just spanX -> clampCameraX halfVisibleWorldWidth targetX spanX
 where
  targetX = posX (playerPos (worldPlayer world))

-- | Clamp a target camera X so the viewport stays inside the world span.
clampCameraX :: Float -> Float -> (Float, Float) -> Float
clampCameraX halfVisible targetX (minX, maxX)
  | maxX <= minX = targetX
  | worldWidth <= visibleWidth = midpoint
  | otherwise = max leftLimit (min rightLimit targetX)
 where
  worldWidth = maxX - minX
  visibleWidth = halfVisible * 2
  midpoint = (minX + maxX) / 2
  leftLimit = minX + halfVisible
  rightLimit = maxX - halfVisible

-- | Authored horizontal span derived from static level platforms.
worldHorizontalSpan :: World -> Maybe (Float, Float)
worldHorizontalSpan world =
  foldl' extendSpan Nothing (platformAabb <$> worldPlatforms world)

extendSpan :: Maybe (Float, Float) -> Aabb -> Maybe (Float, Float)
extendSpan Nothing box =
  Just (aabbMinX box, aabbMaxX box)
extendSpan (Just (minX, maxX)) box =
  Just (min minX (aabbMinX box), max maxX (aabbMaxX box))

halfVisibleWorldWidth :: Float
halfVisibleWorldWidth =
  fromIntegral windowWidth / (2 * renderZoom)
