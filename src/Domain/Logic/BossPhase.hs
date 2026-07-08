module Domain.Logic.BossPhase (
  resolveBossPhases,
)
where

import Data.List (find)
import Data.Maybe (fromMaybe)

import Domain.Logic.BossCatalog (bossDefinitionForKind)
import Domain.Logic.RunBehaviour (playerHorizontalDistance)
import Domain.Model.BossPhase (
  BossDefinition (..),
  BossEventKind (..),
  BossPhaseCondition (..),
  BossPhaseDef (..),
  BossPhaseIndex,
  bossPhaseIndex,
  bossPhaseNumber,
 )
import Domain.Model.Enemy (Enemy (..))
import Domain.Model.EnemyKind (isBossKind)
import Domain.Model.World (World (..))
import Domain.ValueObjects.CombatParams (CombatParams (..))
import Domain.ValueObjects.Health (Health, healthPoints)
import Domain.ValueObjects.HealthRatio (healthAtOrBelowRatio)

resolveBossPhases :: CombatParams -> World -> World -> World
resolveBossPhases cp wBefore wAfter =
  wAfter{worldEnemies = map (resolveBossEnemy cp wBefore wAfter) (worldEnemies wAfter)}

resolveBossEnemy :: CombatParams -> World -> World -> Enemy -> Enemy
resolveBossEnemy cp wBefore wAfter e
  | not (isBossKind (enemyKind e)) = e
  | otherwise =
      case bossDefinitionForKind (enemyKind e) of
        Nothing -> e
        Just def ->
          let healthBefore =
                enemyHealth <$> find ((== enemyId e) . enemyId) (worldEnemies wBefore)
              currentPhase = fromMaybe (bossPhaseIndex 0) (enemyBossPhase e)
              targetPhase = highestSatisfiedPhase cp def wAfter e healthBefore
              -- Las fases solo avanzan (max): un boss nunca vuelve a una fase anterior.
              newPhase = max currentPhase targetPhase
           in if newPhase /= currentPhase
                then applyBossPhase def newPhase e
                else e

highestSatisfiedPhase ::
  CombatParams ->
  BossDefinition ->
  World ->
  Enemy ->
  Maybe Health ->
  BossPhaseIndex
highestSatisfiedPhase cp def w e healthBefore =
  foldl max (bossPhaseIndex 0) $
    [ bossPhaseIndex i
    | (i, phaseDef) <- zip [0 ..] (bossPhases def)
    , conditionsMet cp def w e healthBefore phaseDef
    ]

phaseDefAt :: BossPhaseIndex -> [BossPhaseDef] -> Maybe BossPhaseDef
phaseDefAt idx phases =
  let i = bossPhaseNumber idx
   in if i >= 0 && i < length phases
        then Just (phases !! i)
        else Nothing

conditionsMet ::
  CombatParams ->
  BossDefinition ->
  World ->
  Enemy ->
  Maybe Health ->
  BossPhaseDef ->
  Bool
conditionsMet cp def w e healthBefore phaseDef =
  all (conditionMet cp def w e healthBefore) (phaseConditions phaseDef)

conditionMet ::
  CombatParams ->
  BossDefinition ->
  World ->
  Enemy ->
  Maybe Health ->
  BossPhaseCondition ->
  Bool
conditionMet _ _ _ e _ (HealthAtOrBelowRatio ratio) =
  healthAtOrBelowRatio (enemyHealth e) (enemyMaxHealth e) ratio
conditionMet cp _ w e _ (OnBossEvent PlayerInMeleeRange) =
  playerHorizontalDistance w e <= cpMeleeReach cp
conditionMet _ _ _ e healthBefore (OnBossEvent TookDamageThisFrame) =
  maybe False (\before -> healthPoints before > healthPoints (enemyHealth e)) healthBefore

applyBossPhase :: BossDefinition -> BossPhaseIndex -> Enemy -> Enemy
applyBossPhase def idx e =
  case phaseDefAt idx (bossPhases def) of
    Nothing -> e
    Just phaseDef ->
      e
        { enemyBossPhase = Just idx
        , enemyProgram = phaseProgram phaseDef
        }
