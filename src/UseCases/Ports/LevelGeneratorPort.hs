{-# LANGUAGE DerivingVia #-}

{- | Puerto que genera 'LevelDefinition' a partir de un 'LevelProfile'. La
implementación con 'IO' (API Anthropic) vive en @Adapters/@; los tests usan un stub
puro.

'Nothing' = no pudo generar → fallback al @level{N}.json@ fijo.
-}
module UseCases.Ports.LevelGeneratorPort (
  LevelRole (..),
  LevelProfile (..),
  LevelGeneratorPort (..),
  NoGenerator (..),
)
where

-- Grupo 1 — stdlib / base
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)

-- Grupo 2 — proyecto
import Domain.Model.LevelDefinition (LevelDefinition)

data LevelRole
  = IntroRole
  | ChallengeRole
  | BossRole
  deriving (Eq, Show)

data LevelProfile = LevelProfile
  { profileIndex :: Int
  -- ^ 0-based; el few-shot es @levels/level{profileIndex + 1}.json@.
  , profileRole :: LevelRole
  , profileTheme :: Maybe Text
  }
  deriving (Eq, Show)

class (Monad m) => LevelGeneratorPort m where
  generateLevel :: LevelProfile -> m (Maybe LevelDefinition)

-- | Generador nulo (sin API key, CI offline). Puro vía 'Identity'.
newtype NoGenerator a = NoGenerator {runNoGenerator :: a}
  deriving (Functor, Applicative, Monad) via Identity

instance LevelGeneratorPort NoGenerator where
  generateLevel _ = NoGenerator Nothing
