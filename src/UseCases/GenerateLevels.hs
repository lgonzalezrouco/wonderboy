module UseCases.GenerateLevels (
  defaultProfiles,
  generateCatalog,
)
where

import Data.Text (Text)

import Domain.Model.LevelDefinition (LevelDefinition)
import Domain.Model.LevelRole (LevelRole)
import UseCases.Ports.LevelContentPort (
  LevelContentPort (..),
  LevelProfile (..),
 )

defaultProfiles :: Maybe Text -> [LevelRole] -> [LevelDefinition] -> [LevelProfile]
defaultProfiles theme =
  zipWith3
    (\idx role example -> LevelProfile idx role theme (Just example))
    [0 ..]

generateCatalog ::
  (LevelContentPort m) => [LevelProfile] -> m [Maybe LevelDefinition]
generateCatalog = generateLevels
