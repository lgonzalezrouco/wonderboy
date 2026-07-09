module Domain.Model.MovingPlatform (
  MovingPlatform (..),
  mkMovingPlatform,
  movingPlatformAabb,
  movingPlatformAsPlatform,
  movingPlatformIsHorizontal,
)
where

import Domain.Model.Platform (Platform, platform)
import Domain.ValueObjects.Aabb (Aabb, aabbFromBottomLeft)
import Domain.ValueObjects.Position (Position, posX, posY)
import Domain.ValueObjects.Tolerance (epsilon, near)

data MovingPlatform = MovingPlatform
  { movingPlatformId :: Int
  , movingPlatformPos :: Position
  , movingPlatformWidth :: Float
  , movingPlatformHeight :: Float
  , movingPlatformEndA :: Position
  , movingPlatformEndB :: Position
  , movingPlatformSpeed :: Float
  -- ^ Velocidad de desplazamiento a lo largo de su recorrido, en px/s.
  , movingPlatformTowardB :: Bool
  -- ^ True mientras va hacia endB. Se invierte en cada extremo para el recorrido ping-pong.
  }
  deriving (Eq, Show)

mkMovingPlatform ::
  Int ->
  Position ->
  Float ->
  Float ->
  Position ->
  Position ->
  Float ->
  Bool ->
  Maybe MovingPlatform
mkMovingPlatform pid pos width height endA endB speed towardB
  | width <= 0 || height <= 0 || speed <= 0 = Nothing
  | not (onSegment pos endA endB) = Nothing
  | isHorizontal endA endB || isVertical endA endB =
      Just
        MovingPlatform
          { movingPlatformId = pid
          , movingPlatformPos = pos
          , movingPlatformWidth = width
          , movingPlatformHeight = height
          , movingPlatformEndA = endA
          , movingPlatformEndB = endB
          , movingPlatformSpeed = speed
          , movingPlatformTowardB = towardB
          }
  | otherwise = Nothing

isHorizontal :: Position -> Position -> Bool
isHorizontal a b =
  near (posY a) (posY b) && not (near (posX a) (posX b))

isVertical :: Position -> Position -> Bool
isVertical a b =
  near (posX a) (posX b) && not (near (posY a) (posY b))

onSegment :: Position -> Position -> Position -> Bool
onSegment pos endA endB
  | isHorizontal endA endB =
      near (posY pos) (posY endA)
        && inRange (posX pos) (posX endA) (posX endB)
  | isVertical endA endB =
      near (posX pos) (posX endA)
        && inRange (posY pos) (posY endA) (posY endB)
  | otherwise = False

inRange :: Float -> Float -> Float -> Bool
inRange v a b =
  let lo = min a b
      hi = max a b
   in v >= lo - epsilon && v <= hi + epsilon

movingPlatformIsHorizontal :: MovingPlatform -> Bool
movingPlatformIsHorizontal mp =
  isHorizontal (movingPlatformEndA mp) (movingPlatformEndB mp)

movingPlatformAabb :: MovingPlatform -> Aabb
movingPlatformAabb mp =
  aabbFromBottomLeft
    (movingPlatformPos mp)
    (movingPlatformWidth mp)
    (movingPlatformHeight mp)

movingPlatformAsPlatform :: MovingPlatform -> Platform
movingPlatformAsPlatform mp =
  platform
    (movingPlatformPos mp)
    (movingPlatformWidth mp)
    (movingPlatformHeight mp)
