{- | Orquestación del catálogo de niveles: perfiles estándar y mapeo vía
'LevelContentPort'. Sin 'IO'; espeja 'UseCases.ResolveBehaviours'.
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
import Domain.Model.LevelRole (LevelRole)
import UseCases.Ports.LevelContentPort (
  LevelContentPort (..),
  LevelProfile (..),
 )

{- | Perfiles del run: uno por rol, con su few-shot adjunto.

@roles@ y @examples@ provienen del mismo 'UseCases.RunLayout.runLayout' (los roles
vía 'layoutRoles', los niveles fijos cargados desde 'layoutPaths'), así que tienen
el mismo largo por construcción: cada perfil empareja el rol de un slot con su
nivel fijo, que el generador usa de few-shot. 'zipWith3' los recorre en lockstep;
no hace falta relleno porque cada slot tiene siempre su archivo (un slot sin él
aborta el arranque antes de llegar acá).
-}
defaultProfiles :: Maybe Text -> [LevelRole] -> [LevelDefinition] -> [LevelProfile]
defaultProfiles theme roles examples =
  zipWith3
    (\idx role example -> LevelProfile idx role theme (Just example))
    [0 ..]
    roles
    examples

-- | Un 'Maybe' por perfil; 'Nothing' señala fallback al archivo fijo.
generateCatalog ::
  (LevelContentPort m) => [LevelProfile] -> m [Maybe LevelDefinition]
generateCatalog = generateLevels
