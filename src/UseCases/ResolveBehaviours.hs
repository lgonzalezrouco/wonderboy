{- | Orquestación de la resolución de comportamiento sobre una 'LevelDefinition'.

Recorre los enemigos del nivel, resuelve sus pistas textuales (@behaviourHint@)
al 'BehaviourArchetype' correspondiente vía el puerto 'BehaviourResolverPort', y
rellena el campo @enemyDefBehaviourPreset@ /antes/ de que el build puro
(@Domain.Logic.BuildWorld.buildWorld@) lo consuma. No hay 'IO' acá: todo es
genérico sobre la mónada @m@ del puerto; la impureza (si la hay) la aporta la
instancia concreta en @Adapters/@.

__Precedencia (alineada con la doc de M28 y @buildEnemy@):__

  1. @behaviourPreset@ ya presente → se respeta, __no__ se llama al resolver.
  2. Sin preset y con @behaviourHint@ → se consulta al resolver.
  3. Sin preset ni hint → se deja intacto (el default del kind aplica en build).

__Dedup:__ varios enemigos pueden compartir el par @(EnemyKind, hint)@ (mismo
tipo y misma pista). Resolver cada uno por separado dispararía consultas
redundantes (en el adapter real, llamadas HTTP de más). Por eso primero se
juntan los pares distintos con 'nub' (requiere @'Eq' 'EnemyKind'@), se resuelve
cada uno __una sola vez__ armando una tabla de asociación, y luego se aplica esa
tabla puramente a todos los enemigos. El número de consultas es el de pares
distintos, no el de enemigos.
-}
module UseCases.ResolveBehaviours (
  resolveLevelBehaviours,
)
where

-- Grupo 1 — stdlib / base
import Control.Monad (join)
import Data.List (nub)
import Data.Text qualified as T

-- Grupo 2 — proyecto
import Domain.Model.LevelDefinition (
  EnemyDef (..),
  LevelDefinition (..),
 )
import UseCases.Ports.BehaviourResolverPort (BehaviourResolverPort (..))

{- | Resuelve las pistas de comportamiento de un nivel, rellenando los presets
faltantes según el puerto 'BehaviourResolverPort'.

Devuelve la misma 'LevelDefinition' con @levelEnemies@ posiblemente actualizado:
los enemigos sin preset pero con hint quedan con @enemyDefBehaviourPreset@
seteado al arquetipo resuelto (o 'Nothing' si el resolver no decidió). El resto
de los enemigos —y todos los demás campos del nivel— quedan intactos.
-}
resolveLevelBehaviours ::
  (BehaviourResolverPort m) => LevelDefinition -> m LevelDefinition
resolveLevelBehaviours def = do
  -- Pares distintos (kind, hint) que necesitan resolución: solo enemigos sin
  -- preset explícito (`Nothing <- [...]`), con hint presente (`Just h <- [...]`)
  -- y no en blanco (`not (T.null (T.strip h))`, para no gastar una consulta a la
  -- API en una pista vacía o de solo espacios). `nub` deduplica el mismo par.
  let needs =
        nub
          [ (enemyDefKind e, h)
          | e <- levelEnemies def
          , Nothing <- [enemyDefBehaviourPreset e]
          , Just h <- [enemyDefBehaviourHint e]
          , not (T.null (T.strip h))
          ]
  -- Una consulta por par distinto; se construye la tabla [((kind, hint), Maybe arch)].
  resolved <-
    traverse (\kh@(k, h) -> (,) kh <$> resolveBehaviourHint k h) needs
  -- Aplicación pura de la tabla: solo se toca el caso (sin preset, con hint).
  -- `lookup` da `Maybe (Maybe arch)`; `join` colapsa "par ausente en tabla" y
  -- "resolver devolvió Nothing" en un único `Nothing`.
  let apply e =
        case (enemyDefBehaviourPreset e, enemyDefBehaviourHint e) of
          (Nothing, Just h) ->
            e{enemyDefBehaviourPreset = join (lookup (enemyDefKind e, h) resolved)}
          _ -> e
  pure def{levelEnemies = map apply (levelEnemies def)}
