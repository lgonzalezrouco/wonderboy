{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests de 'mergeGeneratedWithFallbacks', 'selectCatalogSources' y
'bootstrapCatalog' con puerto mockeado (puro vía 'Identity').
-}
module UseCases.BootstrapRunTest where

-- Grupo 1 — stdlib / base
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)

-- Grupo 2 — third-party
import Test.Tasty.HUnit (Assertion, (@?=))

-- Grupo 3 — proyecto
import Domain.Model.EnemyKind (EnemyKind (SnailKind))
import Domain.Model.LevelDefinition (
  BehaviourArchetype (ChaseArchetype),
  EnemyDef (..),
  LevelDefinition (..),
  RectDef (..),
  ResolvedBehaviour (..),
  levelEnemies,
  levelMinScore,
 )
import Domain.ValueObjects.BehaviourTuning (identityTuning)
import Domain.ValueObjects.Position (position)
import UseCases.BootstrapRun (
  bootstrapCatalog,
  mergeGeneratedWithFallbacks,
  selectCatalogSources,
 )
import UseCases.Ports.LevelContentPort (
  LevelContentPort (..),
  LevelProfile (..),
 )

newtype BootstrapStub a = BootstrapStub {runBootstrapStub :: a}
  deriving (Functor, Applicative, Monad) via Identity

cannedGenerated :: [(Int, LevelDefinition)]
cannedGenerated =
  [ (0, levelForIndex 100)
  , (1, levelForIndex 101)
  , (2, levelForIndex 102)
  ]

instance LevelContentPort BootstrapStub where
  generateLevel profile =
    BootstrapStub (lookup (profileIndex profile) cannedGenerated)
  resolveBehaviourHint _ _ =
    BootstrapStub (Just (ResolvedBehaviour ChaseArchetype identityTuning))

runBootstrap :: BootstrapStub a -> a
runBootstrap = runBootstrapStub

catalog :: Bool -> Maybe Text -> [LevelDefinition] -> [LevelDefinition]
catalog gen theme = runBootstrap . bootstrapCatalog gen theme

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

levelForIndex :: Int -> LevelDefinition
levelForIndex idx = baseLevel{levelMinScore = idx}

fileFallbacks :: [LevelDefinition]
fileFallbacks = map levelForIndex [0, 1, 2]

unit_mergeUsesAllGeneratedWhenJust :: Assertion
unit_mergeUsesAllGeneratedWhenJust =
  mergeGeneratedWithFallbacks generated fileFallbacks
    @?= [levelForIndex 10, levelForIndex 11, levelForIndex 12]
 where
  generated :: [Maybe LevelDefinition]
  generated = map Just [levelForIndex 10, levelForIndex 11, levelForIndex 12]

unit_mergeFallsBackOnNothing :: Assertion
unit_mergeFallsBackOnNothing =
  mergeGeneratedWithFallbacks generated fileFallbacks
    @?= [levelForIndex 10, levelForIndex 1, levelForIndex 2]
 where
  generated :: [Maybe LevelDefinition]
  generated = [Just (levelForIndex 10), Nothing, Nothing]

unit_mergePadsShortGeneratedList :: Assertion
unit_mergePadsShortGeneratedList =
  mergeGeneratedWithFallbacks generated fileFallbacks
    @?= [levelForIndex 10, levelForIndex 1, levelForIndex 2]
 where
  generated :: [Maybe LevelDefinition]
  generated = [Just (levelForIndex 10)]

unit_selectCatalogSourcesFileOnly :: Assertion
unit_selectCatalogSourcesFileOnly =
  runBootstrap (selectCatalogSources False Nothing fileFallbacks)
    @?= fileFallbacks

unit_selectCatalogSourcesMergesGenerated :: Assertion
unit_selectCatalogSourcesMergesGenerated =
  runBootstrap (selectCatalogSources True (Just "ice") fileFallbacks)
    @?= [levelForIndex 100, levelForIndex 101, levelForIndex 102]

unit_bootstrapCatalogFileOnlySkipsGenerator :: Assertion
unit_bootstrapCatalogFileOnlySkipsGenerator =
  map levelMinScore (catalog False Nothing fileFallbacks) @?= [0, 1, 2]

unit_bootstrapCatalogResolvesBehaviours :: Assertion
unit_bootstrapCatalogResolvesBehaviours =
  let def = head (catalog False Nothing [levelWithHint])
   in enemyDefBehaviourPreset (head (levelEnemies def))
        @?= Just ChaseArchetype
 where
  levelWithHint :: LevelDefinition
  levelWithHint =
    baseLevel
      { levelEnemies =
          [ EnemyDef
              { enemyDefId = 1
              , enemyDefKind = SnailKind
              , enemyDefPos = position 0 0
              , enemyDefBehaviourPreset = Nothing
              , enemyDefBehaviourHint = Just ("wander" :: Text)
              , enemyDefBehaviourTuning = Nothing
              }
          ]
      }
