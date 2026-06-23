{- | El build aplica el tuning resuelto: 'toughness×' escala la salud del enemigo
(un caracol base de 1 HP con toughness ×3 nace con 3).
-}
module Domain.BuildEnemyTuningTest where

import Test.Tasty.HUnit (Assertion, (@?=))

import Domain.Logic.BuildWorld (buildEnemy)
import Domain.Model.Enemy (enemyHealth, enemyMaxHealth)
import Domain.Model.EnemyKind (EnemyKind (SnailKind))
import Domain.Model.LevelDefinition (
  BehaviourArchetype (ChaseArchetype),
  EnemyDef (..),
 )
import Domain.ValueObjects.BehaviourTuning (BehaviourTuning (..))
import Domain.ValueObjects.Health (healthPoints)
import Domain.ValueObjects.Multiplier (identityMultiplier, mkMultiplier)
import Domain.ValueObjects.Position (position)

unit_buildScalesHealthByToughness :: Assertion
unit_buildScalesHealthByToughness =
  fmap (healthPoints . enemyMaxHealth) (buildEnemy toughSnail) @?= Right 3
 where
  toughSnail =
    EnemyDef
      { enemyDefId = 1
      , enemyDefKind = SnailKind
      , enemyDefPos = position 0 8
      , enemyDefBehaviourPreset = Just ChaseArchetype
      , enemyDefBehaviourHint = Nothing
      , enemyDefBehaviourTuning =
          Just (BehaviourTuning identityMultiplier identityMultiplier (mkMultiplier 3.0))
      }

{- | 'enemyHealth' (salud actual) también refleja el tuning: un enemigo que nace con
×3 de toughness parte con 3 HP actuales, no solo 3 de máximo.
-}
unit_buildScalesCurrentHealthByToughness :: Assertion
unit_buildScalesCurrentHealthByToughness =
  fmap (healthPoints . enemyHealth) (buildEnemy toughSnail) @?= Right 3
 where
  toughSnail =
    EnemyDef
      { enemyDefId = 1
      , enemyDefKind = SnailKind
      , enemyDefPos = position 0 8
      , enemyDefBehaviourPreset = Just ChaseArchetype
      , enemyDefBehaviourHint = Nothing
      , enemyDefBehaviourTuning =
          Just (BehaviourTuning identityMultiplier identityMultiplier (mkMultiplier 3.0))
      }

{- | Enemigo sin tuning (path de identidad): 'enemyHealth' mantiene la salud base (1 HP
para un 'SnailKind' sin arquetipo ni tuning explícito).
-}
unit_noTuningKeepsBaseHealth :: Assertion
unit_noTuningKeepsBaseHealth =
  fmap (healthPoints . enemyHealth) (buildEnemy plainSnail) @?= Right 1
 where
  plainSnail =
    EnemyDef
      { enemyDefId = 2
      , enemyDefKind = SnailKind
      , enemyDefPos = position 0 8
      , enemyDefBehaviourPreset = Nothing
      , enemyDefBehaviourHint = Nothing
      , enemyDefBehaviourTuning = Nothing
      }
