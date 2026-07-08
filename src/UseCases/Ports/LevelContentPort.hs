{-# LANGUAGE DerivingVia #-}

{- | Puerto único de contenido IA: un actor externo (LLM) = un puerto.

Reemplaza @BehaviourResolverPort@ y @LevelGeneratorPort@ con una sola interfaz
que refleja la realidad del dominio: el mismo servicio Anthropic genera niveles y
resuelve arquetipos de comportamiento. Ver @docs\/adr\/0019-level-content-port.md@.

'Nothing' en cualquier método = no pudo producir resultado → el llamador aplica el
fallback correspondiente (nivel fijo del archivo o arquetipo por defecto del kind).
-}
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

{- | Perfil de un slot del catálogo enviado al generador.

'profileIndex' (0-based) identifica el slot y sirve de índice del few-shot
(@levels\/level{n+1}.json@). 'profileRole' indica el rol narrativo que debe
cumplir el nivel generado. 'profileTheme' traslada la directiva temática del
usuario al prompt del LLM.
-}
data LevelProfile = LevelProfile
  { profileIndex :: Int
  -- ^ Slot 0-based dentro del catálogo del run.
  , profileRole :: LevelRole
  -- ^ Rol narrativo y de dificultad del slot.
  , profileTheme :: Maybe Text
  -- ^ Directiva temática opcional del usuario.
  , profileExample :: Maybe LevelDefinition
  -- ^ Nivel fijo del slot, usado como few-shot del generador (ya decodificado,
  --   evita releer el archivo). 'Nothing' ⇒ generar sin ejemplo.
  }
  deriving (Eq, Show)

{- | Puerto de contenido IA: generación de niveles y resolución de arquetipos.

Un método por operación; ambos degradan a 'Nothing' cuando el servicio no puede
atender la solicitud.
-}
class (Monad m) => LevelContentPort m where
  -- | Genera una 'LevelDefinition' para el slot descrito por el perfil.
  -- 'Nothing' → el llamador usa el fallback del archivo fijo.
  generateLevel :: LevelProfile -> m (Maybe LevelDefinition)

  -- | Resuelve el @behaviourHint@ de un enemigo a un 'ResolvedBehaviour'.
  -- 'Nothing' → el llamador usa el arquetipo por defecto del kind.
  resolveBehaviourHint :: EnemyKind -> Text -> m (Maybe ResolvedBehaviour)

  -- | Genera todos los slots del catálogo. El default es secuencial; un
  -- adaptador con 'IO' (p. ej. el de Anthropic) puede sobrescribirlo con
  -- ejecución concurrente, ya que los slots son independientes.
  generateLevels :: [LevelProfile] -> m [Maybe LevelDefinition]
  generateLevels = traverse generateLevel

{- | Puerto nulo (sin API key, CI offline). Ambos métodos devuelven 'Nothing'.
Puro vía 'Identity': sin 'IO', sin excepciones.
-}
newtype NoContent a = NoContent {runNoContent :: a}
  deriving (Functor, Applicative, Monad) via Identity

instance LevelContentPort NoContent where
  generateLevel _ = NoContent Nothing
  resolveBehaviourHint _ _ = NoContent Nothing
