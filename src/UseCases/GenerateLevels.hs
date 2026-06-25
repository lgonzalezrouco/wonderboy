{- | Orquestación del catálogo de niveles: perfiles estándar y mapeo vía
'LevelGeneratorPort'. Sin 'IO'; espeja 'UseCases.ResolveBehaviours'.
-}
module UseCases.GenerateLevels (
  defaultProfiles,
  generateCatalog,
)
where

-- Grupo 1 — stdlib / base
import Data.Text (Text)

-- Grupo 2 — proyecto
import Domain.Model.LevelDefinition (LevelDefinition)
import UseCases.Ports.LevelGeneratorPort (
  LevelGeneratorPort (..),
  LevelProfile (..),
  LevelRole (..),
 )

defaultProfiles :: Maybe Text -> [LevelProfile]
defaultProfiles theme =
  [ LevelProfile 0 IntroRole theme
  , LevelProfile 1 ChallengeRole theme
  , LevelProfile 2 BossRole theme
  ]

-- | Un 'Maybe' por perfil; 'Nothing' señala fallback al archivo fijo.
generateCatalog ::
  (LevelGeneratorPort m) => [LevelProfile] -> m [Maybe LevelDefinition]
generateCatalog = traverse generateLevel
