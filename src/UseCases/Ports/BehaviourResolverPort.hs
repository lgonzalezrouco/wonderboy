{-# LANGUAGE DerivingVia #-}

{- | Puerto de resolución de comportamiento: traduce metadata textual de un enemigo
(@behaviourHint@, texto libre escrito por el autor del nivel) al
'ResolvedBehaviour' canónico que entiende el dominio (arquetipo + tuning).

__Por qué un puerto (typeclass) y no una función concreta:__ la resolución real
puede involucrar 'IO' (una llamada HTTP a un clasificador/SLM de Anthropic). La
arquitectura por capas prohíbe 'IO' en @UseCases/@: la orquestación
('UseCases.ResolveBehaviours') debe permanecer abstracta sobre la mónada @m@. El
puerto invierte la dependencia — @UseCases/@ define la /interfaz/, y la
implementación concreta con 'IO' vive en @Adapters/@; los tests proveen un stub
puro. Así @UseCases/@ nunca importa @Adapters/@.

__Semántica de fallback:__ 'resolveBehaviourHint' devuelve 'Maybe' a propósito.
'Nothing' significa "no pude decidir" (sin API key, falla de red, respuesta no
reconocida): el llamador deja el preset sin tocar y @buildWorld@ usa el default
del kind. El juego siempre queda jugable y el CI corre verde sin red.
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

{- | Puerto que resuelve un @behaviourHint@ (texto libre) al
'ResolvedBehaviour' de un enemigo (arquetipo + tuning).

El 'EnemyKind' se pasa como contexto: la misma pista ("agresivo", "vigila la
puerta") puede mapear a arquetipos distintos según la clase de enemigo, y darle
el kind al clasificador acota el espacio de respuestas válidas.

Devuelve 'Nothing' cuando no puede decidir; el llamador hace fallback al default
del kind (ver semántica de fallback en la doc del módulo). La implementación
concreta ('IO', API Anthropic) vive en @Adapters/@; los tests usan un stub puro.
-}
class (Monad m) => BehaviourResolverPort m where
  -- | Resuelve la pista textual de un enemigo de clase 'EnemyKind' a un
  --   'ResolvedBehaviour', o 'Nothing' si no se puede decidir.
  resolveBehaviourHint :: EnemyKind -> Text -> m (Maybe ResolvedBehaviour)

{- | Resolver nulo: nunca resuelve (siempre 'Nothing').

Se usa cuando no hay API key o se quiere correr offline (CI, tests, smoke runs).
Es __puro__: se deriva la maquinaria monádica vía 'Identity' con @DerivingVia@,
de modo que no hay 'IO' alguno. Esto permite que el orquestador
('UseCases.ResolveBehaviours.resolveLevelBehaviours') corra en un contexto
totalmente puro y deje todos los presets sin tocar, cayendo a los defaults del
kind en el build.

'runNoResolver' extrae el valor envuelto (equivale a 'runIdentity').
-}
newtype NoResolver a = NoResolver {runNoResolver :: a}
  deriving (Functor, Applicative, Monad) via Identity

-- | Instancia del puerto que nunca decide: degrada siempre a 'Nothing'.
instance BehaviourResolverPort NoResolver where
  resolveBehaviourHint _ _ = NoResolver Nothing
