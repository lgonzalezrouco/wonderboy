{- | Orquesta la resolución de @behaviourHint@ sobre una 'LevelDefinition' vía el
puerto 'LevelContentPort', rellenando preset y tuning antes del build puro.

Precedencia: preset explícito > hint resuelto > default del kind. Los pares
@(kind, hint)@ distintos se resuelven una sola vez ('nub') para evitar consultas
redundantes.
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
import UseCases.Ports.LevelContentPort (LevelContentPort (..))

resolveLevelBehaviours ::
  (LevelContentPort m) => LevelDefinition -> m LevelDefinition
resolveLevelBehaviours def = do
  let needs = nub [kh | e <- levelEnemies def, Just kh <- [resolutionKey e]]
  resolved <- traverse resolvePair needs
  pure def{levelEnemies = map (applyResolved resolved) (levelEnemies def)}
 where
  resolutionKey e
    | Nothing <- enemyDefBehaviourPreset e
    , Just h <- enemyDefBehaviourHint e
    , nonBlankHint h =
        Just (enemyDefKind e, h)
    | otherwise =
        Nothing

  nonBlankHint h = not (T.null (T.strip h))

  resolvePair (k, h) = (,) (k, h) <$> resolveBehaviourHint k h

  applyResolved table e
    | Nothing <- enemyDefBehaviourPreset e
    , Just h <- enemyDefBehaviourHint e
    , Just rb <- join (lookup (enemyDefKind e, h) table) =
        e
          { enemyDefBehaviourPreset = Just (rbArchetype rb)
          , enemyDefBehaviourTuning = Just (rbTuning rb)
          }
    | otherwise = e
