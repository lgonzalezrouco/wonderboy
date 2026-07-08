{-# LANGUAGE DerivingVia #-}

module UseCases.Ports.LevelContentPort (
  LevelContentPort (..),
  LevelProfile (..),
  LevelRole (..),
  NoContent (..),
)
where

import Data.Functor.Identity (Identity (..))
import Data.Text (Text)

import Domain.Model.EnemyKind (EnemyKind)
import Domain.Model.LevelDefinition (LevelDefinition, ResolvedBehaviour)
import Domain.Model.LevelRole (LevelRole (..))

data LevelProfile = LevelProfile
  { profileIndex :: Int
  -- ^ Índice de slot (base 0) dentro del catálogo del run.
  , profileRole :: LevelRole
  , profileTheme :: Maybe Text
  , profileExample :: Maybe LevelDefinition
  }
  deriving (Eq, Show)

class (Monad m) => LevelContentPort m where
  -- | Genera un nivel para el slot. 'Nothing' significa que quien llama recurre al archivo fijo.
  generateLevel :: LevelProfile -> m (Maybe LevelDefinition)

  -- | Resuelve el hint de comportamiento de un enemigo. 'Nothing' significa que quien llama usa el archetype por defecto del kind.
  resolveBehaviourHint :: EnemyKind -> Text -> m (Maybe ResolvedBehaviour)

  generateLevels :: [LevelProfile] -> m [Maybe LevelDefinition]
  generateLevels = traverse generateLevel

newtype NoContent a = NoContent {runNoContent :: a}
  deriving (Functor, Applicative, Monad) via Identity

instance LevelContentPort NoContent where
  generateLevel _ = NoContent Nothing
  resolveBehaviourHint _ _ = NoContent Nothing
