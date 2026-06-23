{- | Orquestación de la generación del catálogo de niveles de una partida.

Define los perfiles estándar de una corrida y los mapea, vía el puerto
'LevelGeneratorPort', a un catálogo de niveles generados. No hay 'IO' acá: todo
es genérico sobre la mónada @m@ del puerto; la impureza (si la hay) la aporta la
instancia concreta en @Adapters/@. Espeja la forma de
'UseCases.ResolveBehaviours': @UseCases/@ describe /qué/ generar, el adapter
decide /cómo/.

__Forma del catálogo:__ una partida son tres niveles con progresión de
dificultad —intro, desafío, jefe—. 'defaultProfiles' fija esa secuencia y
propaga el tema del usuario a los tres. 'generateCatalog' los resuelve en orden,
__preservando__ los 'Nothing' (un nivel que no se pudo generar): el llamador en
@Frameworks/@ usa cada 'Nothing' como señal para el fallback granular al
@level{N}.json@ correspondiente.
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

{- | Los tres perfiles estándar de una partida, en orden de progresión.

Arma la secuencia fija intro → desafío → jefe (índices 0/1/2) y propaga el tema
opcional recibido a cada perfil, de modo que la directiva temática del usuario
incide en los tres niveles. Es pura: solo describe los perfiles; no consulta al
generador.
-}
defaultProfiles :: Maybe Text -> [LevelProfile]
defaultProfiles theme =
  [ LevelProfile 0 IntroRole theme
  , LevelProfile 1 ChallengeRole theme
  , LevelProfile 2 BossRole theme
  ]

{- | Genera el catálogo correspondiente a una lista de perfiles.

Una consulta al puerto por perfil, en orden ('traverse' preserva el orden de la
lista). Devuelve un @'Maybe' 'LevelDefinition'@ por perfil: 'Just' para los
niveles generados con éxito y 'Nothing' para los que el generador no pudo
producir. Conservar los 'Nothing' es deliberado — es la señal que
@Frameworks/@ usa para hacer el fallback granular al archivo fijo del nivel.
-}
generateCatalog ::
  (LevelGeneratorPort m) => [LevelProfile] -> m [Maybe LevelDefinition]
generateCatalog = traverse generateLevel
