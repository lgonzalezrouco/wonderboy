{- | Orquestación del catálogo de un run: generación IA con fallback a archivos
fijos y resolución de @behaviourHint@ vía puertos. Sin 'IO'; espeja el arranque
que antes vivía en @Frameworks.Gloss.GameLoop@.
-}
module UseCases.BootstrapRun (
  mergeGeneratedWithFallbacks,
  mergeCatalogSources,
  selectCatalogSources,
  finalizeCatalog,
  bootstrapCatalog,
)
where

-- Grupo 1 — stdlib / base
import Data.Maybe (fromMaybe)
import Data.Text (Text)

-- Grupo 2 — proyecto
import Domain.Model.LevelDefinition (LevelDefinition)
import UseCases.GenerateLevels (defaultProfiles, generateCatalog)
import UseCases.Ports.BehaviourResolverPort (BehaviourResolverPort (..))
import UseCases.Ports.LevelGeneratorPort (LevelGeneratorPort (..))
import UseCases.ResolveBehaviours (resolveLevelBehaviours)

{- | Combina definiciones generadas con fallbacks de archivo por índice.

@Just gen@ gana; @Nothing@ usa el fallback pre-cargado en la misma posición.
La lista generada se rellena con @Nothing@ hasta la longitud de los fallbacks.
-}
mergeGeneratedWithFallbacks ::
  [Maybe LevelDefinition] ->
  [LevelDefinition] ->
  [LevelDefinition]
mergeGeneratedWithFallbacks generated fileFallbacks =
  zipWith fromMaybe fileFallbacks (padTo n generated)
 where
  n = length fileFallbacks

{- | Fusiona generación con fallbacks, o devuelve solo fallbacks si está desactivada.

El adaptador pasa 'generateCatalogIO'; los tests usan 'generateCatalog' vía el puerto.
-}
mergeCatalogSources ::
  (Applicative m) =>
  Bool ->
  m [Maybe LevelDefinition] ->
  [LevelDefinition] ->
  m [LevelDefinition]
mergeCatalogSources False _ fileFallbacks = pure fileFallbacks
mergeCatalogSources True genAction fileFallbacks =
  mergeGeneratedWithFallbacks <$> genAction <*> pure fileFallbacks

{- | Elige fuentes del catálogo antes de resolver comportamientos.

Con @generateEnabled@, consulta 'LevelGeneratorPort' y fusiona con fallbacks;
si no, devuelve los fallbacks sin tocar el puerto.
-}
selectCatalogSources ::
  (LevelGeneratorPort m) =>
  Bool ->
  Maybe Text ->
  [LevelDefinition] ->
  m [LevelDefinition]
selectCatalogSources generateEnabled theme =
  mergeCatalogSources
    generateEnabled
    (generateCatalog (defaultProfiles theme))

-- | Resuelve @behaviourHint@ en cada nivel del catálogo.
finalizeCatalog ::
  (BehaviourResolverPort m) =>
  [LevelDefinition] ->
  m [LevelDefinition]
finalizeCatalog = traverse resolveLevelBehaviours

{- | Catálogo completo listo para el run: fuentes + resolución de comportamiento.

@fileFallbacks@ son definiciones ya decodificadas (una por slot del run).
-}
bootstrapCatalog ::
  (LevelGeneratorPort m, BehaviourResolverPort m) =>
  Bool ->
  Maybe Text ->
  [LevelDefinition] ->
  m [LevelDefinition]
bootstrapCatalog generateEnabled theme fileFallbacks =
  selectCatalogSources generateEnabled theme fileFallbacks
    >>= finalizeCatalog

padTo :: Int -> [Maybe a] -> [Maybe a]
padTo n xs = take n (xs ++ repeat Nothing)
