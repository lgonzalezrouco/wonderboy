{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 'resolveLevelBehaviours' con puerto stub puro (precedencia y dedup).
module UseCases.ResolveBehavioursTest where

import Data.Functor.Identity (Identity (..))
import Data.Text (Text)

import Test.Tasty.HUnit (Assertion, (@?=))

import Domain.Model.EnemyKind (EnemyKind (SnailKind))
import Domain.Model.LevelDefinition (
  BehaviourArchetype (ChaseArchetype, GuardArchetype, PatrolArchetype),
  EnemyDef (..),
  LevelDefinition (..),
  RectDef (..),
  ResolvedBehaviour (..),
 )
import Domain.ValueObjects.BehaviourTuning (identityTuning)
import Domain.ValueObjects.Position (position)
import UseCases.Ports.LevelContentPort (LevelContentPort (..))
import UseCases.ResolveBehaviours (resolveLevelBehaviours)

newtype Stub a = Stub {runStub :: a}
  deriving (Functor, Applicative, Monad) via Identity

cannedTable :: [(Text, BehaviourArchetype)]
cannedTable =
  [ ("hunts the player", ChaseArchetype)
  , ("guards the gate", GuardArchetype)
  ]

instance LevelContentPort Stub where
  generateLevel _ = Stub Nothing
  resolveBehaviourHint _ hint =
    Stub (fmap (`ResolvedBehaviour` identityTuning) (lookup hint cannedTable))

resolve :: LevelDefinition -> LevelDefinition
resolve = runStub . resolveLevelBehaviours

baseLevel :: LevelDefinition
baseLevel =
  LevelDefinition
    { levelMinScore = 0
    , levelSpawn = position 0 0
    , levelPlatforms = []
    , levelMovingPlatforms = []
    , levelEnemies = []
    , levelPickups = []
    , levelFallingHazards = []
    , levelCrumblingPlatforms = []
    , levelBossArena = Nothing
    , levelExit = RectDef{rectPos = position 0 0, rectWidth = 1, rectHeight = 1}
    }

mkEnemy :: Int -> Maybe BehaviourArchetype -> Maybe Text -> EnemyDef
mkEnemy eid preset hint =
  EnemyDef
    { enemyDefId = eid
    , enemyDefKind = SnailKind
    , enemyDefPos = position 0 0
    , enemyDefBehaviourPreset = preset
    , enemyDefBehaviourHint = hint
    , enemyDefBehaviourTuning = Nothing
    }

resolvedEnemies :: [EnemyDef] -> [EnemyDef]
resolvedEnemies enemies = levelEnemies (resolve baseLevel{levelEnemies = enemies})

firstPreset :: [EnemyDef] -> Maybe BehaviourArchetype
firstPreset enemies = case resolvedEnemies enemies of
  e : _ -> enemyDefBehaviourPreset e
  [] -> Nothing

unit_resolvesKnownHintFillsPreset :: Assertion
unit_resolvesKnownHintFillsPreset =
  firstPreset [mkEnemy 1 Nothing (Just "hunts the player")]
    @?= Just ChaseArchetype

unit_explicitPresetTakesPrecedence :: Assertion
unit_explicitPresetTakesPrecedence =
  firstPreset [mkEnemy 1 (Just PatrolArchetype) (Just "hunts the player")]
    @?= Just PatrolArchetype

unit_noHintNoPresetStaysNothing :: Assertion
unit_noHintNoPresetStaysNothing =
  firstPreset [mkEnemy 1 Nothing Nothing] @?= Nothing

unit_unknownHintStaysNothing :: Assertion
unit_unknownHintStaysNothing =
  firstPreset [mkEnemy 1 Nothing (Just "blah blah desconocido")] @?= Nothing

unit_blankHintStaysNothing :: Assertion
unit_blankHintStaysNothing =
  firstPreset [mkEnemy 1 Nothing (Just "   ")] @?= Nothing

unit_sameKindHintResolvesAllEnemies :: Assertion
unit_sameKindHintResolvesAllEnemies =
  map
    enemyDefBehaviourPreset
    ( resolvedEnemies
        [ mkEnemy 1 Nothing (Just "guards the gate")
        , mkEnemy 2 Nothing (Just "guards the gate")
        ]
    )
    @?= [Just GuardArchetype, Just GuardArchetype]
