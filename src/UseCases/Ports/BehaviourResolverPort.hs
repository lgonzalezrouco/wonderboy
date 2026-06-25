{-# LANGUAGE DerivingVia #-}

{- | Puerto que traduce @behaviourHint@ a 'ResolvedBehaviour'. La implementación con
'IO' (API Anthropic) vive en @Adapters/@; los tests usan un stub puro.

'Nothing' = no pudo decidir → el build usa el default del kind.
-}
module UseCases.Ports.BehaviourResolverPort (
  BehaviourResolverPort (..),
  NoResolver (..),
)
where

-- Grupo 1 — stdlib / base
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)

-- Grupo 2 — proyecto
import Domain.Model.EnemyKind (EnemyKind)
import Domain.Model.LevelDefinition (ResolvedBehaviour)

class (Monad m) => BehaviourResolverPort m where
  resolveBehaviourHint :: EnemyKind -> Text -> m (Maybe ResolvedBehaviour)

-- | Resolver nulo (sin API key, CI offline). Puro vía 'Identity'.
newtype NoResolver a = NoResolver {runNoResolver :: a}
  deriving (Functor, Applicative, Monad) via Identity

instance BehaviourResolverPort NoResolver where
  resolveBehaviourHint _ _ = NoResolver Nothing
