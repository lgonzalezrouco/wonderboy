{-# LANGUAGE OverloadedStrings #-}

{- | Driver IO del arranque del run (el lado 'IO' de 'UseCases.BootstrapRun'):
carga archivos, lee variables de entorno y orquesta
'UseCases.BootstrapRun.bootstrapCatalog' vía 'AnthropicContent' o 'NoContent'.
Toda la política de slots vive en 'UseCases.BootstrapRun'; el wiring top-level
del juego vive en 'Frameworks.Gloss.GameLoop', que invoca a este módulo.

Este módulo es el único lugar donde se construye el 'AnthropicEnv'; con clave
usa 'AnthropicContent', sin clave usa 'NoContent' (degradación pura, sin 'IO').
-}
module Adapters.BootstrapRunIO (
  bootstrapCatalogIO,
)
where

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Data.Text qualified as T

import Network.HTTP.Client.TLS (newTlsManager)

import Adapters.Anthropic.Client (
  AnthropicClient (..),
  FeatureCfg (..),
  nonEmptyApiKey,
 )
import Adapters.Anthropic.Content (
  AnthropicEnv (..),
  runAnthropicContent,
 )
import Adapters.LevelFile (readLevelFile)
import Domain.Model.LevelDefinition (LevelDefinition)
import Paths_wonderboy_hs (getDataFileName)
import UseCases.BootstrapRun (bootstrapCatalog)
import UseCases.GameMonad (GameError (..))
import UseCases.LoadLevel (decodeLevelDefinition)
import UseCases.Ports.LevelContentPort (NoContent (..))

defaultModel :: T.Text
defaultModel = "claude-haiku-4-5"

-- | 30 s por intento de generación (nivel completo, @max_tokens@ 4096).
generatorTimeoutMicros :: Int
generatorTimeoutMicros = 30 * 1000 * 1000

-- | 10 s por consulta de resolución (respuesta corta, @max_tokens@ 64).
resolverTimeoutMicros :: Int
resolverTimeoutMicros = 10 * 1000 * 1000

{- | Pre-carga el catálogo del run: generación IA + resolución de arquetipos, o
degradación pura a archivos fijos + defaults del kind si no hay API key.

Con clave Anthropic: delega a 'bootstrapCatalog' vía 'AnthropicContent'.
Sin clave: delega a 'bootstrapCatalog' vía 'NoContent' (sin 'IO', sin red).
-}
bootstrapCatalogIO :: [FilePath] -> IO [LevelDefinition]
bootstrapCatalogIO paths = do
  fileFallbacks <- traverse loadDefFromFile paths
  genOn <- lookupEnv "WONDERBOY_GENERATE_LEVELS"
  theme <- lookupEnv "WONDERBOY_WORLD_PROMPT"
  mKey <- nonEmptyApiKey <$> lookupEnv "ANTHROPIC_API_KEY"
  let generateEnabled = isEnabled genOn
      themeText = T.pack <$> theme
  case mKey of
    Nothing -> do
      hPutStrLn
        stderr
        "[bootstrap] ANTHROPIC_API_KEY ausente o vacía; uso archivos fijos + arquetipos por defecto."
      pure (runNoContent (bootstrapCatalog generateEnabled themeText fileFallbacks))
    Just key -> do
      manager <- newTlsManager
      mGenModel <- lookupEnv "WONDERBOY_GENERATOR_MODEL"
      mResModel <- lookupEnv "WONDERBOY_RESOLVER_MODEL"
      mGenDebug <- lookupEnv "WONDERBOY_GENERATOR_DEBUG"
      mResDebug <- lookupEnv "WONDERBOY_RESOLVER_DEBUG"
      let client = AnthropicClient manager key "https://api.anthropic.com/v1/messages"
          genCfg =
            FeatureCfg
              { fcModel = maybe defaultModel T.pack mGenModel
              , fcTimeoutMicros = generatorTimeoutMicros
              , fcDebug = isEnabled mGenDebug
              }
          resCfg =
            FeatureCfg
              { fcModel = maybe defaultModel T.pack mResModel
              , fcTimeoutMicros = resolverTimeoutMicros
              , fcDebug = isEnabled mResDebug
              }
          env = AnthropicEnv client genCfg resCfg
      runAnthropicContent env (bootstrapCatalog generateEnabled themeText fileFallbacks)

{- | Interpreta una variable de entorno como booleano.

Ausente o vacía ⇒ desactivado. Los valores @0@, @false@, @no@ y @off@
(insensibles a mayúsculas y espacios) también desactivan; cualquier otro valor
no vacío activa. Evita el sorprendente @WONDERBOY_GENERATE_LEVELS=0@ que antes
__activaba__ la generación.
-}
isEnabled :: Maybe String -> Bool
isEnabled = maybe False truthy
 where
  truthy raw = case map toLower (trim raw) of
    "" -> False
    "0" -> False
    "false" -> False
    "no" -> False
    "off" -> False
    _ -> True
  trim = dropWhileEnd isSpace . dropWhile isSpace

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
