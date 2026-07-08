module UseCases.RunLayout (
  RunSlot (..),
  runLayout,
  layoutRoles,
  layoutPaths,
)
where

import Domain.Model.LevelRole (LevelRole (..))

data RunSlot = RunSlot
  { slotRole :: LevelRole
  , slotFile :: FilePath
  }
  deriving (Eq, Show)

runLayout :: [RunSlot]
runLayout =
  [ RunSlot IntroRole "levels/level1.json"
  , RunSlot ChallengeRole "levels/level2.json"
  , RunSlot BossRole "levels/level3.json"
  ]

layoutRoles :: [LevelRole]
layoutRoles = map slotRole runLayout

layoutPaths :: [FilePath]
layoutPaths = map slotFile runLayout
