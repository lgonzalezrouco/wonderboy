{-# LANGUAGE DerivingVia #-}

{- | Puerto de generación de niveles: dado un perfil de nivel ('LevelProfile')
—posición en el catálogo, rol en la progresión de dificultad y tema opcional—
produce una 'LevelDefinition' generada por IA, lista para que el build puro
(@Domain.Logic.BuildWorld.buildWorld@) la consuma.

__Por qué un puerto (typeclass) y no una función concreta:__ la generación real
involucra 'IO' (una llamada HTTP a la API de Anthropic que devuelve el JSON del
nivel). La arquitectura por capas prohíbe 'IO' en @UseCases/@: el orquestador
('UseCases.GenerateLevels') debe permanecer abstracto sobre la mónada @m@ del
puerto. El puerto invierte la dependencia — @UseCases/@ define la /interfaz/, y
la implementación concreta con 'IO' vive en @Adapters/@; los tests proveen un
stub puro. Así @UseCases/@ nunca importa @Adapters/@.

__Semántica de fallback:__ 'generateLevel' devuelve 'Maybe' a propósito.
'Nothing' significa "no pude generar este nivel" (sin API key, falla de red,
JSON inválido, decode o build fallido tras reintentar): el llamador hace
fallback granular y carga @levels/level{N}.json@ en su lugar, de modo que la
partida nunca se rompe. El juego siempre queda jugable y el CI corre verde sin
red. Espeja la forma de 'UseCases.Ports.BehaviourResolverPort'.
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

{- | Rol de un nivel dentro de la progresión de dificultad del catálogo de una
partida.

Se reifica como tipo (en vez de, por ejemplo, un 'Int' de dificultad) porque el
rol gobierna /qué reglas de contenido/ aplica el adapter al armar el prompt:
'IntroRole' pide plataformas fijas y enemigos básicos; 'ChallengeRole' suma
móviles y hazards; 'BossRole' pide una arena con un jefe. Nombrar cada rol hace
explícito el concepto de dominio y deja el pattern-match exhaustivo.
-}
data LevelRole
  = -- | Nivel introductorio: geometría simple, enemigos básicos, pickups y salida.
    IntroRole
  | -- | Nivel de desafío: agrega plataformas móviles y hazards.
    ChallengeRole
  | -- | Nivel de jefe: arena sellada con un único jefe.
    BossRole
  deriving (Eq, Show)

{- | Perfil de generación de un nivel: todo lo que el generador necesita para
producir una 'LevelDefinition' concreta.

Reúne la posición en el catálogo, el rol en la progresión y la directiva
temática opcional del usuario. Es un valor de dominio puro: no contiene 'IO' ni
detalles del transporte HTTP; el adapter lo traduce a un prompt.
-}
data LevelProfile = LevelProfile
  { profileIndex :: Int
  -- ^ Índice 0-based en el catálogo. El few-shot que el adapter adjunta al
  --   prompt es @levels/level{profileIndex + 1}.json@ (los archivos son
  --   1-based), por eso se guarda el índice y no la ruta del archivo.
  , profileRole :: LevelRole
  -- ^ Rol en la progresión de dificultad; selecciona las reglas de contenido.
  , profileTheme :: Maybe Text
  -- ^ Directiva temática opcional (env var @WONDERBOY_WORLD_PROMPT@). 'Nothing'
  --   cuando el usuario no pidió un tema; el prompt entonces no lo menciona.
  }
  deriving (Eq, Show)

{- | Puerto que genera la 'LevelDefinition' de un nivel a partir de su perfil.

Devuelve 'Nothing' cuando no puede generar un nivel válido; el llamador hace
fallback granular al @level{N}.json@ correspondiente (ver semántica de fallback
en la doc del módulo). La implementación concreta ('IO', API Anthropic) vive en
@Adapters/@; los tests usan un stub puro sobre 'Identity'.
-}
class (Monad m) => LevelGeneratorPort m where
  -- | Genera la definición del nivel descripto por el perfil, o 'Nothing' si
  --   no se pudo producir un nivel válido.
  generateLevel :: LevelProfile -> m (Maybe LevelDefinition)

{- | Generador nulo: nunca genera (siempre 'Nothing').

Se usa cuando no hay API key o se quiere correr offline (CI, tests, smoke runs).
Es __puro__: se deriva la maquinaria monádica vía 'Identity' con @DerivingVia@,
de modo que no hay 'IO' alguno. Esto permite que el orquestador
('UseCases.GenerateLevels.generateCatalog') corra en un contexto totalmente
puro y devuelva un catálogo de puros 'Nothing', con lo que @Frameworks/@ cae al
catálogo de archivos fijos.

Nota (lección del resolver): @deriving ... via Identity@ requiere importar el
__constructor__ @Identity (..)@; sin el @(..)@ GHC tira GHC-10283
("data constructor Identity not in scope").

'runNoGenerator' extrae el valor envuelto (equivale a 'runIdentity').
-}
newtype NoGenerator a = NoGenerator {runNoGenerator :: a}
  deriving (Functor, Applicative, Monad) via Identity

-- | Instancia del puerto que nunca genera: degrada siempre a 'Nothing'.
instance LevelGeneratorPort NoGenerator where
  generateLevel _ = NoGenerator Nothing
