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
import Domain.Model.LevelRole (LevelRole (..))
import UseCases.Ports.LevelContentPort (
  LevelContentPort (..),
  LevelProfile (..),
 )

{- | Perfiles estándar del run, uno por rol, con su few-shot adjunto.

@examples@ son los niveles fijos ya decodificados (uno por slot, en orden); cada
uno se adjunta como 'profileExample' del perfil correspondiente para que el
generador lo use de few-shot sin releer el archivo. Slots sin ejemplo disponible
quedan en 'Nothing'.
-}
defaultProfiles :: Maybe Text -> [LevelDefinition] -> [LevelProfile]
defaultProfiles theme examples =
  zipWith3
    (\idx role example -> LevelProfile idx role theme example)
    [0 ..]
    [IntroRole, ChallengeRole, BossRole]
    (map Just examples ++ repeat Nothing)

-- | Un 'Maybe' por perfil; 'Nothing' señala fallback al archivo fijo.
generateCatalog ::
  (LevelContentPort m) => [LevelProfile] -> m [Maybe LevelDefinition]
generateCatalog = generateLevels
