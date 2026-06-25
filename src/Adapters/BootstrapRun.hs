{- | Adaptador de arranque del run: carga archivos, lee variables de entorno y
compone 'generateCatalogIO' con 'resolveLevelIO'. Toda la política de slots vive
en 'UseCases.BootstrapRun'.
-}
module Adapters.BootstrapRun (
  bootstrapCatalogIO,
)
where

-- Grupo 1 — stdlib / base
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Data.Text qualified as T

-- Grupo 2 — proyecto
import Adapters.BehaviourResolver (resolveLevelIO)
import Adapters.LevelFile (readLevelFile)
import Adapters.LevelGenerator (generateCatalogIO)
import Domain.Model.LevelDefinition (LevelDefinition)
import Paths_wonderboy_hs (getDataFileName)
import UseCases.BootstrapRun (mergeCatalogSources)
import UseCases.GameMonad (GameError (..))
import UseCases.LoadLevel (decodeLevelDefinition)

-- | Pre-carga el catálogo del run (generación IA o archivos fijos + resolver).
bootstrapCatalogIO :: [FilePath] -> IO [LevelDefinition]
bootstrapCatalogIO paths = do
  fileFallbacks <- traverse loadDefFromFile paths
  genOn <- lookupEnv "WONDERBOY_GENERATE_LEVELS"
  theme <- lookupEnv "WONDERBOY_WORLD_PROMPT"
  mergeCatalogSources
    (isEnabled genOn)
    (generateCatalogIO (T.pack <$> theme))
    fileFallbacks
    >>= traverse resolveLevelIO
 where
  isEnabled = maybe False (not . null)

loadDefFromFile :: FilePath -> IO LevelDefinition
loadDefFromFile relPath = do
  path <- getDataFileName relPath
  readResult <- readLevelFile path
  case readResult of
    Left err -> exitWithError err
    Right txt ->
      case decodeLevelDefinition txt of
        Left (GameError err) -> exitWithError err
        Right def -> pure def

exitWithError :: String -> IO a
exitWithError err = hPutStrLn stderr ("Error: " ++ err) >> exitFailure
