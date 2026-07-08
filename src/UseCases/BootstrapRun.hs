module UseCases.BootstrapRun (
  mergeGeneratedWithFallbacks,
  mergeCatalogSources,
  selectCatalogSources,
  bootstrapCatalog,
)
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Domain.Model.LevelDefinition (LevelDefinition)
import UseCases.GenerateLevels (defaultProfiles, generateCatalog)
import UseCases.Ports.LevelContentPort (LevelContentPort (..))
import UseCases.ResolveBehaviours (resolveLevelBehaviours)
import UseCases.RunLayout (layoutRoles)

-- | Fusiona por índice: un 'Just' generado pisa el fallback del archivo. 'Nothing' o un slot faltante conserva el fallback.
mergeGeneratedWithFallbacks ::
  [Maybe LevelDefinition] ->
  [LevelDefinition] ->
  [LevelDefinition]
mergeGeneratedWithFallbacks generated fileFallbacks =
  zipWith fromMaybe fileFallbacks (padTo n generated)
 where
  n = length fileFallbacks

mergeCatalogSources ::
  (Applicative m) =>
  Bool ->
  m [Maybe LevelDefinition] ->
  [LevelDefinition] ->
  m [LevelDefinition]
mergeCatalogSources False _ fileFallbacks = pure fileFallbacks
mergeCatalogSources True genAction fileFallbacks =
  mergeGeneratedWithFallbacks <$> genAction <*> pure fileFallbacks

selectCatalogSources ::
  (LevelContentPort m) =>
  Bool ->
  Maybe Text ->
  [LevelDefinition] ->
  m [LevelDefinition]
selectCatalogSources generateEnabled theme fileFallbacks =
  mergeCatalogSources
    generateEnabled
    (generateCatalog (defaultProfiles theme layoutRoles fileFallbacks))
    fileFallbacks

bootstrapCatalog ::
  (LevelContentPort m) =>
  Bool ->
  Maybe Text ->
  [LevelDefinition] ->
  m [LevelDefinition]
bootstrapCatalog generateEnabled theme fileFallbacks =
  selectCatalogSources generateEnabled theme fileFallbacks
    >>= traverse resolveLevelBehaviours

padTo :: Int -> [Maybe a] -> [Maybe a]
padTo n xs = take n (xs ++ repeat Nothing)
