module Domain.Model.LevelRole (
  LevelRole (..),
)
where

data LevelRole
  = IntroRole
  | ChallengeRole
  | BossRole
  deriving (Eq, Show)
