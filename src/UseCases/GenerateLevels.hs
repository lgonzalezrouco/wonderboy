{- | Orquestación del catálogo de niveles: perfiles estándar y mapeo vía
'LevelContentPort'. Sin 'IO'; espeja 'UseCases.ResolveBehaviours'.
-}
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

{- | Perfiles del run: uno por rol, con su few-shot adjunto.

@roles@ y @examples@ vienen del mismo 'UseCases.RunLayout.runLayout', así que
tienen igual longitud y 'zipWith3' los recorre en lockstep sin relleno.
-}
defaultProfiles :: Maybe Text -> [LevelRole] -> [LevelDefinition] -> [LevelProfile]
defaultProfiles theme =
  zipWith3
    (\idx role example -> LevelProfile idx role theme (Just example))
    [0 ..]

-- | Un 'Maybe' por perfil; 'Nothing' señala fallback al archivo fijo.
generateCatalog ::
  (LevelContentPort m) => [LevelProfile] -> m [Maybe LevelDefinition]
generateCatalog = generateLevels
