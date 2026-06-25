{- | Orquestación de la resolución de comportamiento sobre una 'LevelDefinition'.

Recorre los enemigos del nivel, resuelve sus pistas textuales (@behaviourHint@)
al 'ResolvedBehaviour' correspondiente vía el puerto 'BehaviourResolverPort', y
rellena los campos @enemyDefBehaviourPreset@ y @enemyDefBehaviourTuning@ /antes/ de que el build puro
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
  ResolvedBehaviour (..),
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
  let needs = nub [kh | e <- levelEnemies def, Just kh <- [resolutionKey e]]
  resolved <- traverse resolvePair needs
  pure def{levelEnemies = map (applyResolved resolved) (levelEnemies def)}
 where
  -- Solo enemigos sin preset, con hint no vacío (evita consultas inútiles a la API).
  resolutionKey e
    | Nothing <- enemyDefBehaviourPreset e
    , Just h <- enemyDefBehaviourHint e
    , nonBlankHint h =
        Just (enemyDefKind e, h)
    | otherwise =
        Nothing

  nonBlankHint h = not (T.null (T.strip h))

  resolvePair (k, h) = (,) (k, h) <$> resolveBehaviourHint k h

  -- `join` colapsa "par ausente" y "resolver devolvió Nothing" en un único `Nothing`.
  applyResolved table e
    | Nothing <- enemyDefBehaviourPreset e
    , Just h <- enemyDefBehaviourHint e =
        let mrb = join (lookup (enemyDefKind e, h) table)
         in e
              { enemyDefBehaviourPreset = rbArchetype <$> mrb
              , enemyDefBehaviourTuning = rbTuning <$> mrb
              }
    | otherwise =
        e
