module Adapters.Gloss.Camera (
  cameraXForWorld,
  clampCameraX,
  worldHorizontalSpan,
)
where

import Data.List (foldl')

import Control.Applicative ((<|>))

import Adapters.Gloss.Config (renderZoom, windowWidth)
import Domain.Logic.BossArena (bossArenaSealed)
import Domain.Model.BossArena (bossArenaLeft, bossArenaRight)
import Domain.Model.Platform (platformAabb)
import Domain.Model.Player (playerPos)
import Domain.Model.World (World (..))
import Domain.ValueObjects.Aabb (Aabb, aabbMaxX, aabbMinX)
import Domain.ValueObjects.Position (posX)

cameraXForWorld :: World -> Float
cameraXForWorld world =
  let targetX = posX (playerPos (worldPlayer world))
   in maybe
        targetX
        (clampCameraX halfVisibleWorldWidth targetX)
        (bossArenaCameraSpan world <|> worldHorizontalSpan world)

bossArenaCameraSpan :: World -> Maybe (Float, Float)
bossArenaCameraSpan w =
  case worldBossArena w of
    Just arena
      | bossArenaSealed w ->
          Just (bossArenaLeft arena, bossArenaRight arena)
    _ -> Nothing

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
