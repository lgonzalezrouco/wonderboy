module Domain.ValueObjects.Frames (
  Frames,
  frames,
  frameCount,
  noFrames,
  tickFrames,
  hasFramesLeft,
)
where

import GHC.Generics (Generic)

newtype Frames = Frames Int
  deriving (Eq, Ord, Show, Generic)

frames :: Int -> Frames
frames n = Frames (max 0 n)

frameCount :: Frames -> Int
frameCount (Frames n) = n

noFrames :: Frames
noFrames = Frames 0

tickFrames :: Frames -> Frames
tickFrames (Frames n) = Frames (max 0 (n - 1))

hasFramesLeft :: Frames -> Bool
hasFramesLeft (Frames n) = n > 0
